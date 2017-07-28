open Batteries;;
open Jhupllib;;
open Analysis_construct_cfg;;
open Analysis_lookup;;
open Analysis_grammar;;
open Analysis_types;;
open Analysis_lexical_relations;;
open Uid_ctx;;

open Logger_utils;;
open Lamia_ast_pretty;;
open Pp_utils;;

let add_to_log = make_logger "Python Analysis Toploop";;
set_default_logging_level `debug;;
set_logging_level_for "Analysis_lookup_edge_functions" `debug;;

type analysis =
  {
    pds: pds;
    prog: block;
    relations: relation_map_record;
    uids: uid_context;
  }
;;

let parse_and_analyze lexbuf : analysis =
  let modl = Python2_ast_pipeline.parse_to_normalized lexbuf 0 true in
  let ctx = Unique_name_ctx.create_new_name_ctx 0 "lamia$" in
  let annot_block = Lamia_converter.convert_module ctx modl in
  let uid_block, uid_ctx = Lamia_converter.annot_to_uid annot_block in
  add_to_log `debug @@ "Lamia program:\n" ^ pp_to_string pp_block_top uid_block;
  let abstract_block, _ = Analysis_lift_ast.lift_block_top uid_block in
  let pds, relations = construct_analysis abstract_block in
  {
    pds = pds;
    prog = abstract_block;
    relations = relations;
    uids = uid_ctx;
  }
;;

let rec get_input () =
  let target_str = input_line stdin in
  if String.length target_str = 0 then
    get_input ()
  else
    target_str
;;

let get_python_prog () =
  print_endline "Enter a python filename:";
  print_endline "-------------";
  let filename = get_input () in
  input_file filename
;;

let rec get_user_query () =
  print_endline "Enter a variable name:";
  print_endline "-------------";
  let target = get_input () in
  print_endline "Enter a line number:";
  print_endline "-------------";
  let line = get_input () in
  target, line
;;

let get_starting_state analysis line =
  let Block(stmts) = analysis.prog in
  (* We want the annot nearest the input line number but not before it *)
  (* TODO: We can do this faster by using the stmt_map returned from lift_block_top *)
  let rec find_first_valid stmts =
    match stmts with
    | [] -> None
    | stmt::rest ->
      let Statement(u, d) = stmt in
      let annot = Uid_ctx.get_annotation_from_uid analysis.uids u in
      let pos = Python2_ast.Pos.to_pos annot in
      if pos.Lexing.pos_lnum > line then
        Some(stmt)
      else
        let in_subordinate =
          match d with
          | While(_, Block(body)) ->
            find_first_valid body
          | Try_except(Block(body), _, Block(orelse))
          | Let_conditional_value(_, _, Block(body), Block(orelse))
          | Let_conditional_memory(_, _, Block(body), Block(orelse)) ->
            begin
              match find_first_valid body with
              | None -> find_first_valid orelse
              | Some(s) -> Some(s)
            end
          | _ -> None
        in
        match in_subordinate with
        | None -> find_first_valid rest
        | Some(s) -> Some(s)
  in
  match find_first_valid stmts with
  | None ->
    Program_state.End
  | Some(stmt) ->
    Program_state.Stmt(stmt)
;;

let rec extract_value (analysis : analysis) starting_state target_str v =
  let scopeval =
    match v with
    | Object_value bindings -> bindings
    | _ -> raise @@ Utils.Invariant_failure "Python variable was not an object!"
  in
  (* See if the scope we got has the target variable bound *)
  try
    let obj_loc = AbstractStringMap.find (String_exact target_str) scopeval in
    (* If so, look up the possible object values and return them *)
    let obj_values, new_pds =
      lookup_memory_location starting_state obj_loc analysis.pds
    in
    obj_values, {analysis with pds = new_pds}
  with
  | Not_found ->
    (* The object wasn't in our local scope. *)
    match starting_state with
    | Program_state.Stmt(s) ->
      begin
        (* Try to move up in scope, and do another lookup from there. Do
           this by looking at the first statement in our block, and starting our
           second lookup from there (i.e. from right before it executed). *)
        (* Note: this does some extra work since we walk back to the beginning
           of our block, and only function blocks create a new python scope.
           So if we're inside, e.g. a while inside a function, we'll lookup
           from the starting point, then from just before the while, then
           from just before the function call: only the last one will return
           a new value for the scope. This isn't too bad because the pds
           automatically caches lookups for a variable from a given point, so
           the lookup from the beginning of the while won't cost much *)
        match Stmt_map.find s analysis.relations.double_left with
        | None ->
          Enum.empty (), analysis
        | Some(prev) ->
          make_query analysis (Program_state.Stmt(prev)) target_str
      end
    | _ -> Enum.empty (), analysis

and make_query analysis starting_state target_str =
  let target = Memory_variable("&scope") in
  add_to_log `debug @@ "Performing lookup for " ^
                       pp_to_string pp_memory_variable target ^
                       " from " ^
                       pp_to_string Program_state.pp starting_state;
  let results, new_pds =
    lookup_memory starting_state target analysis.pds
  in
  let analysis = {analysis with pds = new_pds} in
  let all_possible_values, final_analysis =
    Enum.fold
      (fun (old_values, analysis) value ->
         let new_values, new_analysis =
           extract_value analysis starting_state target_str value
         in
         (Enum.append new_values old_values, new_analysis)
      )
      (Enum.empty (), analysis)
      results
  in
  all_possible_values, final_analysis
;;

let main () =
  let prog = get_python_prog () in
  let analysis = parse_and_analyze prog in
  let rec loop analysis =
    let target_str, line = get_user_query () in
    let starting_state =
      get_starting_state analysis (int_of_string line)
    in
    let results, analysis =
      make_query analysis starting_state target_str
    in
    print_endline @@
    "The possible values of " ^ target_str ^ " are " ^
    Jhupllib_pp_utils.pp_to_string (Jhupllib_pp_utils.pp_list pp_value) (List.of_enum results)
    ;
    loop analysis
  in
  loop analysis
;;

main ();;
