open Batteries;;
open Python2_cfg;;
open Python2_pds;;
open Python2_pds.Reachability.Stack_action.T;;
open Python2_pds.Reachability.Terminus.T;;
module Ast = Python2_normalized_ast;;
open Ast;;


let literal_to_answer l =
  match l with
  | Ast.Num (n,_,_) -> Python2_pds.Num n
  | Ast.Str (s,_,_) -> Python2_pds.Str s
  | Ast.Bool (b,_,_) -> Python2_pds.Bool b
;;

let create_edge_function (e : Control_cfg.edge) (state : Reachability.State.t)
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
          
        end
        ;
        (* Rule 1a *)
        begin
          let%orzero
            Program_point(Assign(id,SimpleExpr(Literal(v,_,_),_,_),_,_)) = a1
          in
          (* x = v *)
          return ([Pop(Var(id));Push(Ans(literal_to_answer v))],
                  Static_terminus(Cfg_node(a0)))
        end;

      ]
  in transitions_to_add
;;
