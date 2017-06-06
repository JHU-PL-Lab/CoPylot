open Batteries;;
open Python2_normalized_ast;;

type vertex =
  | Program_point of stmt
  | Start
  | End
[@@deriving eq, ord, show, to_yojson]
;;

module Lexical_vertex =
struct
  type t = vertex
  let compare = compare_vertex
  let pp = pp_vertex
  let equal = equal_vertex
end
;;

module Control_vertex =
struct
  type t = vertex
  let compare = compare_vertex
  let pp = pp_vertex
  let equal = equal_vertex
end
;;

module Lexical_cfg = Graph.Make(Lexical_vertex);;

module Control_cfg = Graph.Make(Control_vertex);;

(* TODO: Add a signature and, y'know, the rest of the functionality *)
module Cfg =
struct
  type t = Cfg of Lexical_cfg.t * Control_cfg.t;; (* TODO: Make this a record? *)

  let add_control_edge (e : Control_cfg.edge) (curr : t) : t =
    begin
      match curr with
      | Cfg (lex, ctrl) -> Cfg (lex, Control_cfg.add_edge e ctrl)
    end
  ;;

  (* TODO: Deal with stuff like if statements that have subordinate stmt lists *)
  let create_lexical_cfg (m : modl) =
    let stmt_list_to_vertex_list (lst : stmt list) : vertex list =
      List.map (fun stmt -> Program_point(stmt)) lst
    in
    let vertex_list_to_edge_list (lst : vertex list) : Lexical_cfg.edge list =
      List.map2
        (fun v1 v2 -> Lexical_cfg.Edge(v1, v2))
        ([Start] @ lst)
        (lst @ [End])
    in
    match m with
    | Module (stmts, _) ->
      let v_lst = stmt_list_to_vertex_list stmts in
      let e_lst = vertex_list_to_edge_list v_lst in
      List.fold_left
        (fun gph e -> Lexical_cfg.add_edge e gph)
        Lexical_cfg.empty
        e_lst
  ;;

  let create (m : modl) : t =
    Cfg(create_lexical_cfg m, Control_cfg.empty)
  ;;

end;;
