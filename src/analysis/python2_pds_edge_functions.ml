open Batteries;;
open Python2_cfg;;
open Python2_pds;;
open Python2_pds.Reachability.Stack_action.T;;
open Python2_pds.Reachability.Terminus.T;;
open Python2_pds.Dph.Untargeted_dynamic_pop_action;;
module Ast = Python2_normalized_ast;;
open Ast;;

let literal_to_answer l =
  match l with
  | Ast.Num (n,_,_) -> Python2_pds.Num n
  | Ast.Str (s,_,_) -> Python2_pds.Str s
  | Ast.Bool (b,_,_) -> Python2_pds.Bool b
;;

let create_edge_function
    (e : Control_cfg.edge) (state : Reachability.State.t)
  : (Reachability.Stack_action.t list * Reachability.Terminus.t) Enum.t =
  let Control_cfg.Edge (a1,a0) = e in
  let zero = Enum.empty in
  let%orzero Cfg_node(s) = state in
  [%guard (equal_vertex s a0)];
  let open Option.Monad in
  let zero () = None in
  let transitions_to_add = Enum.filter_map identity @@ List.enum
      [
        (* enum of pairs of (list of actions,terminus) *)
        (* Rule 0a *)
        begin
          return ([],Dynamic_terminus(Goto_value_state))
        end
        ;
        (* Rule 1a *)
        begin
          let%orzero
            Program_point(Assign(id,SimpleExpr(Literal(v,_,_),_,_),_,_)) = a1
          in
          return ([Pop(Var(id)); Push(Ans(literal_to_answer v))],
                  Static_terminus(Cfg_node(a0)))
        end
        ;
        (* Rule 1b *)
        begin
          let%orzero
            Program_point(Assign(id, SimpleExpr(Literal(_,_,_),_,_),_,_)) = a1
          in
          return ([Pop_dynamic_targeted(Dph.Pop_then_push_any_variable_but(Var(id)))],
                  Static_terminus(Cfg_node(a1)))
        end
        ;
        (* Variable Aliasing *)
        begin
          let%orzero
            Program_point(Assign(id, SimpleExpr(Name(id2,_,_),_,_),_,_)) = a1
          in (* TODO: Make sure id2 is bound *)
          return ([Pop(Var(id)); Push(Var(id2))],
                  Static_terminus(Cfg_node(a1)))
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
