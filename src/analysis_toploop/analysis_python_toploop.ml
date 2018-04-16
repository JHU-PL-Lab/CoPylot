open Batteries;;
open Jhupllib;;
open Python2_ast_types;;
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
set_default_logging_level `warn;;
let level = `debug;;
set_logging_level_for "Analysis_lookup_edge_functions" level;;
set_logging_level_for "Analysis_construct_cfg" level;;
set_logging_level_for "Python Analysis Toploop" level;;

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
  let lybie_block = Lybie_converter.convert_module ctx modl.body in
  let expanded_block = Lybie_expander.expand_macros_block ctx lybie_block in
  let uid_block, uid_ctx = Lybie_expander.annot_to_uid expanded_block in
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
          | While(_, Block(body), Block(orelse))
          | Let_conditional_value(_, _, Block(body), Block(orelse))
          | Let_conditional_memory(_, _, Block(body), Block(orelse)) ->
            begin
              match find_first_valid body with
              | Some(s) -> Some(s)
              | None -> find_first_valid orelse
            end
          | Try_except(Block(body), _, Block(handler), Block(orelse)) ->
            begin
              match find_first_valid body with
              | Some(s) -> Some(s)
              | None ->
                begin
                  match find_first_valid handler with
                  | Some(s) -> Some(s)
                  | None -> find_first_valid orelse
                end
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

let make_query (analysis : analysis) starting_state target_str =
  add_to_log `debug @@ "Performing lookup for " ^ target_str ^ " from " ^
                       pp_to_string Program_state.pp starting_state;
  let results, new_pds =
    lookup_to_starvalue starting_state target_str analysis.pds
  in
  let new_analysis = {analysis with pds = new_pds} in
  results, new_analysis
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
