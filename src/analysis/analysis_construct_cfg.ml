open Batteries;;
open Analysis_types;;
open Analysis_grammar;;
open Analysis_lexical_relations;;
open Analysis_lookup;;
open Analysis_construct_cfg_monad;;

open Jhupllib;;
open Logger_utils;;
open Pp_utils;;

let add_to_log = make_logger "Analysis cfg construction";;
set_default_logging_level `debug;;

type analysis_construction =
  {
    pds : pds;
    cfg : Cfg.t;
  }
;;

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
let add_edge relations analysis edge =
  let open Cfg in
  let open Program_state in
  let open Cfg_monad in
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
      let yvals, new_pds = lookup_memory v2 y analysis.pds in
      update_pds new_pds @@
      let%bind yval = pick_enum yvals in
      if yval = Boolean_value(true) then
        return @@ Edge(v2, Stmt(List.hd body))
      else if yval = Boolean_value(false) then
        return @@ Edge(v2, Advance(s))
      else
        raise @@ Jhupllib.Utils.Invariant_failure "While loop got non-boolean lamia value"
    end
    ;
    (* If stmt (value) *)
    begin
      let%orzero Stmt(Statement(_, d)) = v2 in
      let%orzero Let_conditional_value(_, test, Block(body), Block(orelse)) = d in
      let testvals, new_pds = lookup_value v2 test analysis.pds in
      update_pds new_pds @@
      let%bind testval = pick_enum testvals in
      if testval = Boolean_value(true) then
        return @@ Edge(v2, Stmt(List.hd body))
      else if testval = Boolean_value(false) then
        return @@ Edge(v2, Stmt(List.hd orelse))
      else
        raise @@ Jhupllib.Utils.Invariant_failure "If stmt got non-boolean lamia value"
    end
    ;
    (* If stmt (memory) *)
    begin
      let%orzero Stmt(Statement(_, d)) = v2 in
      let%orzero Let_conditional_memory(_, test, Block(body), Block(orelse)) = d in
      let testvals, new_pds = lookup_value v2 test analysis.pds in
      update_pds new_pds @@
      let%bind testval = pick_enum testvals in
      if testval = Boolean_value(true) then
        return @@ Edge(v2, Stmt(List.hd body))
      else if testval = Boolean_value(false) then
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
      let%orzero Stmt(Statement(u, d)) = v2 in
      let%orzero Let_call_function(_, func, _) = d in
      let Value_variable (v) = func in
      add_to_log `debug @@ "Calling function " ^ v ^ " at line " ^ string_of_int u;
      let funcvals, new_pds = lookup_value v2 func analysis.pds in
      add_to_log `debug @@ "Number of results: " ^ string_of_int @@ Enum.count funcvals;
      update_pds new_pds @@
      let%bind funcval = pick_enum funcvals in
      let%orzero Function_value(_, Block(body)) = funcval in
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
      let%orzero Statement(_, While _) = block_start in
      return @@ Edge(v2, Stmt(block_start))
    end
    ;
    (* Advance (try) *)
    begin
      let%orzero Advance(s) = v2 in
      let%orzero None = Stmt_map.find s relations.left in
      let%orzero Some(block_start) = Stmt_map.find s relations.down in
      let%orzero Statement(_, Try_except _) = block_start in
      return @@ Edge(v2, Advance(block_start))
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
    (* If result (value) *)
    begin
      let%orzero Ifresult(s) = v2 in
      let%orzero Some(block_start) = Stmt_map.find s relations.down in
      let%orzero Statement(_, Let_conditional_value _) = block_start in
      return @@ Edge(v2, Advance(block_start))
    end
    ;
    (* If result (memory) *)
    begin
      let%orzero Ifresult(s) = v2 in
      let%orzero Some(block_start) = Stmt_map.find s relations.down in
      let%orzero Statement(_, Let_conditional_memory _) = block_start in
      return @@ Edge(v2, Advance(block_start))
    end
    ;
    (* If result (in a while loop) *)
    begin
      let%orzero Ifresult(s) = v2 in
      let%orzero Some(block_start) = Stmt_map.find s relations.down in
      let%orzero Statement(_, While _) = block_start in
      return @@ Edge(v2, Ifresult(block_start))
    end
    ;
    (* If result (in a try block) *)
    begin
      let%orzero Ifresult(s) = v2 in
      let%orzero Some(block_start) = Stmt_map.find s relations.down in
      let%orzero Statement(_, Try_except _) = block_start in
      return @@ Edge(v2, Ifresult(block_start))
    end
    ;
    (* Return stmt *)
    begin
      let%orzero Program_state.Return(s) = v2 in
      let%orzero Some(block_start) = Stmt_map.find s relations.down in
      let%orzero Some(func_start) = Stmt_map.find s relations.double_left in
      let edges_to_start = Cfg.edges_to (Stmt(func_start)) analysis.cfg in
      let%bind edge = pick_enum edges_to_start in
      let%orzero Edge(Stmt(s0), _) = edge in
      match block_start with
      (* Returning from top level in the function *)
      | Statement(_, Let_expression(_, Function_expression _)) ->
        return @@ Edge(v2, Advance(s0))
      (* Returning from a subordinate block *)
      | _ ->
        return @@ Edge(v2, Return(s0))
    end
    ;
    (* Raise stmt *)
    begin
      let%orzero Program_state.Raise(s) = v2 in
      let%orzero Some(block_start) = Stmt_map.find s relations.down in
      let Statement(_, d) = block_start in
      match d with
      | Try_except _ ->
        let%orzero Some(block_first) = Stmt_map.find s relations.double_left in
        let edges_to_start = Cfg.edges_to (Stmt(block_first)) analysis.cfg in
        let%bind edge = pick_enum edges_to_start in
        let%orzero Edge(Stmt(s0), _) = edge in
        begin
          match s0 with
          (* We're inside the body of a try_except; move to the handler *)
          | Statement(_, Try_except(_, _, Block(orelse))) ->
            return @@ Edge(v2, Stmt(List.hd orelse))

          (* We entered this block from a raise; we're in the exn handler already *)
          | Statement(_, Analysis_types.Raise _) ->
            return @@ Edge(v2, Raise(block_start))

          (* If this happens everything is broken *)
          | _ ->
            raise @@ Jhupllib.Utils.Invariant_failure "If stmt got non-boolean lamia value"
        end
      | _ ->
        return @@ Edge(v2, Raise(block_start))
    end
    ;
  ]
  |> List.enum
  |> fold_cfg_monad_enum analysis.pds
;;

let rec add_all_edges relations analysis =
  let existing_edges = Cfg.edges_of analysis.cfg in
  let edges_to_add, analysis =
    Enum.fold
      (fun (old_edges, analysis) e ->
         let new_edges, new_pds = add_edge relations analysis e in
         Enum.append old_edges new_edges, {analysis with pds = new_pds}
      )
      (Enum.empty (), analysis)
      existing_edges
  in
  let new_analysis =
    Enum.fold
      (fun analysis edge ->
         {
           pds = Analysis_lookup.add_cfg_edge edge analysis.pds;
           cfg = Cfg.add_edge edge analysis.cfg;
         }
      )
      analysis edges_to_add
  in
  if Cfg.num_edges new_analysis.cfg = Cfg.num_edges analysis.cfg then
    analysis
  else
    add_all_edges relations new_analysis
;;


let simple_pp_edge fmt edge =
  let open Format in

  let simple_pp_statement fmt statement =
    let Statement(u, _) = statement in
    fprintf fmt "%d" u
  in

  let simple_pp_state fmt state =
    match state with
    | Program_state.Stmt(s) -> fprintf fmt "Stmt(%a)" simple_pp_statement s
    | Program_state.Advance(s) -> fprintf fmt "Advance(%a)" simple_pp_statement s
    | Program_state.Raise(s) -> fprintf fmt "Raise(%a)" simple_pp_statement s
    | Program_state.Return(s) -> fprintf fmt "Return(%a)" simple_pp_statement s
    | Program_state.Ifresult(s) -> fprintf fmt "Ifresult(%a)" simple_pp_statement s
    | Program_state.Start -> fprintf fmt "Start"
    | Program_state.End -> fprintf fmt "End"
  in

  let Cfg.Edge(s1, s2) = edge in
  fprintf fmt "%a -> %a"
    simple_pp_state s1
    simple_pp_state s2
;;

let construct_analysis (prog : block) : pds * relation_map_record =
  let relations = construct_all_relation_maps prog in
  let Block(stmts) = prog in
  (* TODO: Throw useful error if prog is empty *)
  let base_cfg =
    Cfg.empty
    |> Cfg.add_edge @@ Cfg.Edge(Program_state.Start, Program_state.Stmt(List.hd stmts))
  in
  let empty_pds = Analysis_lookup.empty relations in
  let base_analysis = {pds = empty_pds; cfg = base_cfg} in
  let final_analysis = add_all_edges relations base_analysis in
  add_to_log `debug @@ "Final cfg:\n" ^ pp_to_string (pp_set simple_pp_edge (fun () -> Cfg.edges_of final_analysis.cfg)) ();
  final_analysis.pds, relations
;;
