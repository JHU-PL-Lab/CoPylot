open Batteries;;
open Jhupllib;;
open Nondeterminism;;
open Python2_cfg;;
open Python2_pds;;
open Python2_pds.Reachability.Stack_action.T;;
open Python2_pds.Reachability.Terminus.T;;
open Python2_pds.Dph.Untargeted_dynamic_pop_action;;
open Python2_ast_types;;
module Ast = Python2_abstract_ast;;
open Ast;;

let literal_to_answer l =
  match l with
  | Ast.Num (n)       -> Python2_pds.Num n
  | Ast.Str (s)       -> Python2_pds.Str s
  | Ast.Bool (b)      -> Python2_pds.Bool b
  | Ast.Builtin (b)   -> Python2_pds.Builtin b
  | Ast.FunctionVal _ -> failwith "Functional answer type NYI"
  | Ast. NoneVal      -> Python2_pds.NoneVal
;;

let create_edge_function
    (e : Control_cfg.edge) (state : Reachability.State.t)
  : (Reachability.Stack_action.t list * Reachability.Terminus.t) Enum.t =
  let Control_cfg.Edge (a1,a0) = e in
  let zero = Enum.empty in
  let%orzero Cfg_node(s) = state in
  [%guard (equal_vertex s a0)];
  let open Nondeterminism_monad in
  let transitions_to_add = Enum.concat @@ Enum.map enum @@ List.enum
      [
        (* enum of pairs of (list of actions,terminus) *)

        (* Pop a value, go to the value state *)
        begin
          return ([],Dynamic_terminus(Goto_value_state))
        end
        ;
        (* If we're looking for a variable at the start node, it's undefined *)
        begin
          let%orzero
            Start = a1
          in
          return ([], Dynamic_terminus(Pop_undefined_variable))
        end
        ;
        (* Assignment to a variable from a literal *)
        begin
          let%orzero
            Program_point({uid=_;
                           exception_target=_;
                           multi=_;
                           body=Assign
                               (id,{uid=_;
                                   exception_target=_;
                                   multi=_;
                                   body=Literal(v)})}) = a1
          in
          (* Assignment to target variable*)
          let relevant = ([Pop(Var(id)); Push(Ans(literal_to_answer v))],
                          Static_terminus(Cfg_node(a0))) in
          (* Assignment to irrelevant variable *)
          let irrelevant = ([Pop_dynamic_targeted(Dph.Pop_then_push_any_variable_but(Var(id)))],
                            Static_terminus(Cfg_node(a1))) in
          pick_enum @@ List.enum [relevant; irrelevant]
        end
        ;
        (* TODO: We have the exact same code here *)
        (* Variable Aliasing *)
        begin
          let%orzero
            Program_point({uid=_;
                           exception_target=_;
                           multi=_;
                           body=Assign
                               (id,{uid=_;
                                   exception_target=_;
                                   multi=_;
                                   body=Name(id2)})}) = a1
          in
          (* Alias to the variable we're looking for *)
          let relevant =  ([Pop(Var(id)); Push(Var(id2))],
                           Static_terminus(Cfg_node(a1))) in
          (* Assignment to some other variable *)
          let irrelevant = ([Pop_dynamic_targeted(Dph.Pop_then_push_any_variable_but(Var(id)))],
                            Static_terminus(Cfg_node(a1))) in
          pick_enum @@ List.enum [relevant; irrelevant]
        end
        ;
      ]
  in transitions_to_add
;;

let value_loop_edge_function (state : Reachability.State.t)
  : (Reachability.Stack_action.t list * Reachability.Terminus.t) Enum.t =
  let zero = Enum.empty in
  let%orzero Value_node(_) = state in
  Enum.singleton ([Pop(Bottom)], Static_terminus(state))
;;
