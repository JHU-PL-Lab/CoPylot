open Batteries;;
open Jhupllib;;
open Nondeterminism;;
open Python2_cfg;;
open Python2_pds;;
open Python2_ast_types;;
open Python2_abstract_ast;;
open Python2_analysis_result_structure;;
open Python2_normalization_ctx;;

(* TODO: This can be made much more efficient *)
let is_active (v : vertex) (gph : Control_cfg.t) =
  let rec breadth_first_search start seen : vertex list =
    let seen = start::seen in
    let neighbors = Control_cfg.succs start gph in
    let unseen_neighbors =
      Enum.filter (fun v -> not (List.mem v seen)) neighbors
    in
    let result =
      unseen_neighbors
      |> Enum.fold (fun s v -> breadth_first_search v s) seen
    in result
  in (* End definition of breadth_first_search *)
  List.mem v (breadth_first_search Start [])
;;

let apply_rules (curr : analysis_result) (e : Lexical_cfg.edge)
  : Control_cfg.edge Enum.t =
  (* TODO: Get code from renormalization branch *)
  let lookup_values = Python2_pds.query_pds curr.analysis_pds in
  let Lexical_cfg.Edge (v1, v2) = e in
  let Cfg.Cfg(_, ctrl) = curr.analysis_cfg in
  let zero = Enum.empty in
  [%guard (is_active v1 ctrl)]; (* Never add an edge out of an inactive node *)
  let open Nondeterminism_monad in
  let edges_to_add = Enum.concat @@ Enum.map Nondeterminism_monad.enum @@ List.enum
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
            Program_point(
              {body=
                 Assign(_,
                        {body=
                           Literal(_)
                        ;_})
              ;_}) = v1
          in
          return (Control_cfg.Edge(v1, v2))
        end
        ;
        (* Variable Aliasing *)
        begin
          let%orzero
            Program_point(
              {uid=_;
               exception_target=except;
               multi=_;
               body=Assign
                   (
                     _,{uid=_;
                        exception_target=_;
                        multi=_;
                        body=Name(id)})}) = v1
          in
          let%bind v =
            pick_enum @@ Python2_pds.Answer_set.enum @@ lookup_values v1 id
          in
          match v with
          | Undefined ->
            begin
              match except with
              | None -> raise @@ Utils.Not_yet_implemented "Exception outside of try"
              | Some (uid) ->
                let handler = Counter_hashtbl.find curr.analysis_uid_map uid in
                return @@ Control_cfg.Edge(v1, Program_point(handler))
            end
          | _ -> return (Control_cfg.Edge(v1, v2))
        end
        ;
      ]
  in edges_to_add
;;

(* Returns a list of edges which the rules say we should add to the Control
   cfg, but which are not currently in it. *)
let get_edges_to_add (curr : analysis_result) : Control_cfg.edge Enum.t =
  let Cfg.Cfg (lex, ctrl) = curr.analysis_cfg in
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
