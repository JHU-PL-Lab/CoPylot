open Batteries;;
open Python2_cfg;;
open Cfg;;
open Python2_normalized_ast;;

let is_active (_ : vertex) (_ : Control_cfg.t) = true;; (* TODO *)

let apply_rules (curr : t) (e : Lexical_cfg.edge) : Control_cfg.edge Enum.t =
  let Lexical_cfg.Edge (v1, v2) = e in
  let Cfg(_, ctrl) = curr in
  let zero = Enum.empty in
  [%guard (is_active v1 ctrl)]; (* Never add an edge out of an inactive node *)
  let open Option.Monad in
  let zero () = None in
  let edges_to_add = Enum.filter_map identity @@ List.enum
      [
        (* Start Rule *)
        begin
          let%orzero
            Start = v1
          in
          return (Control_cfg.Edge(v1, v2))
        end
        ;
        (* Assignment from literal *)
        begin
          let%orzero
            Program_point(Assign(_,SimpleExpr(Literal(_),_,_),_,_)) = v1
          in
          return (Control_cfg.Edge(v1, v2))
        end
        ;
      ]
  in edges_to_add
;;

(* Returns a list of edges which the rules say we should add to the Control
   cfg, but which are not currently in it. *)
let get_edges_to_add (curr : t) : Control_cfg.edge Enum.t =
  match curr with
  | Cfg (lex, ctrl) ->
    let rec collect_edges_from_vertex (v : vertex) : Control_cfg.edge Enum.t =
      let outgoing_edges = Lexical_cfg.edges_from v lex in
      let new_edges_to_add = Enum.map (apply_rules curr) outgoing_edges in
      let edges_to_add =
        Enum.fold
          Enum.append
          (Enum.empty ())
          new_edges_to_add
      in
      let successors = Lexical_cfg.succs v lex in
      (* If there are cycles in the lexical cfg this will never end *)
      let all_edges_to_add =
        Enum.fold
          (fun en v -> Enum.append en (collect_edges_from_vertex v))
          edges_to_add
          successors
      in
      all_edges_to_add
    in (* End definition of collect_edges_from_vertex *)
    (* Filter out edges that already exist *)
    Enum.filter
      (fun e -> not (Control_cfg.has_edge e ctrl))
      (collect_edges_from_vertex Start)
;;
