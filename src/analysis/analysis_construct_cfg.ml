open Batteries;;
open Analysis_types;;
open Analysis_grammar;;
open Analysis_lexical_relations;;
open Jhupllib_nondeterminism;;

let skippable s =
  let Statement(_, d) = s in
  match d with
  | Let_expression _
  | Let_alloc _
  | Let_alias_value _
  | Let_alias_memory _
  | Let_binding_update _
  | Let_binding_access _
  | Let_list_access _
  | Let_list_slice _
  | Store _
  | Let_get _
  | Let_is _
  | Let_unop _
  | Let_binop _
    -> true
  | _
    -> false
;;

let lookup _ _ _ = failwith "LOOKUP NYI";;

let add_edge relations analysis graph edge =
  let open Cfg in
  let open Program_state in
  let open Nondeterminism_monad in
  let Edge(_, v2) = edge in
  [
    (* Skip *)
    begin
      let%orzero Stmt(s) = v2 in
      [%guard skippable s];
      return @@ Edge(v2, Advance(s))
    end
    ;
    (* While loop *)
    begin
      let%orzero Stmt(s) = v2 in
      let%orzero Statement(_, While(y, Block(body))) = s in
      let%bind yval = lookup analysis graph y in
      if yval = Boolean(true) then
        return @@ Edge(v2, Stmt(List.hd body))
      else if yval = Boolean(false) then
        return @@ Edge(v2, Advance(s))
      else
        raise @@ Jhupllib.Utils.Invariant_failure "While loop got non-boolean lamia value"
    end
    ;
    (* If stmt (value) *)
    begin
      let%orzero Stmt(Statement(_, d)) = v2 in
      let%orzero Let_conditional_value(_, test, Block(body), Block(orelse)) = d in
      let%bind testval = lookup analysis graph test in
      if testval = Boolean(true) then
        return @@ Edge(v2, Stmt(List.hd body))
      else if testval = Boolean(false) then
        return @@ Edge(v2, Stmt(List.hd orelse))
      else
        raise @@ Jhupllib.Utils.Invariant_failure "If stmt got non-boolean lamia value"
    end
    ;
    (* If stmt (memory) *)
    begin
      let%orzero Stmt(Statement(_, d)) = v2 in
      let%orzero Let_conditional_memory(_, test, Block(body), Block(orelse)) = d in
      let%bind testval = lookup analysis graph test in
      if testval = Boolean(true) then
        return @@ Edge(v2, Stmt(List.hd body))
      else if testval = Boolean(false) then
        return @@ Edge(v2, Stmt(List.hd orelse))
      else
        raise @@ Jhupllib.Utils.Invariant_failure "If stmt got non-boolean lamia value"
    end
    ;
    (* If result (value) *)
    begin
      let%orzero Stmt(s) = v2 in
      let%orzero Statement(_, If_result_value(_)) = s in
      return @@ Edge(v2, Ifresult(s))
    end
    ;
    (* If result (memory) *)
    begin
      let%orzero Stmt(s) = v2 in
      let%orzero Statement(_, If_result_memory(_)) = s in
      return @@ Edge(v2, Ifresult(s))
    end
    ;
    (* Function call *)
    begin
      let%orzero Stmt(Statement(_, d)) = v2 in
      let%orzero Let_call_function(_, func, _) = d in
      let%bind funcval = lookup analysis graph func in
      let%orzero Function(_, Block(body)) = funcval in
      return @@ Edge(v2, Stmt(List.hd body))
    end
    ;
    (* Return stmt *)
    begin
      let%orzero Stmt(s) = v2 in
      let%orzero Statement(_, Analysis_types.Return _) = s in
      return @@ Edge(v2, Return(s))
    end
    ;
    (* Try block *)
    begin
      let%orzero Stmt(Statement(_, d)) = v2 in
      let%orzero Try_except(Block(body), _, _) = d in
      return @@ Edge(v2, Stmt(List.hd body))
    end
    ;
    (* Raise stmt *)
    begin
      let%orzero Stmt(s) = v2 in
      let%orzero Statement(_, Analysis_types.Raise _) = s in
      return @@ Edge(v2, Raise(s))
    end
    ;
    (* Advance *)
    begin
      let%orzero Advance(s) = v2 in
      let%orzero Some(next_stmt) = Stmt_map.find s relations.left in
      return @@ Edge(v2, Stmt(next_stmt))
    end
    ;
    (* Advance (while) *)
    begin
      let%orzero Advance(s) = v2 in
      let%orzero None = Stmt_map.find s relations.left in
      let%orzero Some(block_start) = Stmt_map.find s relations.down in
      let%orzero Statement(_, While(_)) = block_start in
      return @@ Edge(v2, Stmt(block_start))
    end
    ;
    (* Advance (try) *)
    begin
      let%orzero Advance(s) = v2 in
      let%orzero None = Stmt_map.find s relations.left in
      let%orzero Some(block_fst) = Stmt_map.find s relations.double_left in
      let edges_to_fst = Cfg.edges_to (Stmt(block_fst)) graph in
      (* TODO: add of_enum to Nondeterminism_monad directly? *)
      let%bind Edge(s0, _) = Nondeterminism_monad.of_list @@ List.of_enum edges_to_fst in
      return @@ Edge(v2, s0)
    end
    ;
    (* Advance (exn) *)
    begin
      let%orzero Advance(s) = v2 in
      let%orzero None = Stmt_map.find s relations.left in
      let%orzero Some(block_fst) = Stmt_map.find s relations.double_left in
      let edges_to_fst = Cfg.edges_to (Stmt(block_fst)) graph in
      (* TODO: add of_enum to Nondeterminism_monad directly? *)
      let%bind Edge(s0, _) = Nondeterminism_monad.of_list @@ List.of_enum edges_to_fst in
      let%orzero Advance(s') = s0 in
      return @@ Edge(v2, Stmt(s'))
    end
    ;
    (* Advance (EOF) *)
    begin
      let%orzero Advance(s) = v2 in
      let%orzero None = Stmt_map.find s relations.left in
      let%orzero None = Stmt_map.find s relations.down in
      return @@ Edge(v2, End)
    end
    ;

  ]
  |> List.enum
  |> Enum.map Nondeterminism_monad.enum
  |> Enum.concat
;;

let rec add_all_edges relations analysis graph =
  let existing_edges = Cfg.edges_of graph in
  let edges_to_add =
    Enum.concat @@ Enum.map (add_edge relations analysis graph) existing_edges
  in
  if Enum.is_empty edges_to_add then
    graph
  else
    let new_graph = Enum.fold (fun g e -> Cfg.add_edge e g) graph edges_to_add in
    let new_analysis = analysis in (* TODO: update analysis *)
    add_all_edges relations new_analysis new_graph
;;

let construct_cfg (prog : block) : Cfg.t =
  let relations = construct_all_relation_maps prog in
  let Block(stmts) = prog in
  (* TODO: Throw useful error if prog is empty *)
  let base_cfg =
    Cfg.empty
    |> Cfg.add_edge @@ Cfg.Edge(Program_state.Start, Program_state.Stmt(List.hd stmts))
  in
  let empty_analysis = () in (* TODO: Analysis type *)
  add_all_edges relations empty_analysis base_cfg
;;
