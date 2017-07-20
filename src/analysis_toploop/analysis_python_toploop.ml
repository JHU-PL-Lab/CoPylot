open Batteries;;
open Analysis_construct_cfg;;
open Analysis_lookup;;
open Analysis_grammar;;
open Analysis_types;;

let parse_and_analyze lexbuf =
  let modl = Python2_ast_pipeline.parse_to_normalized lexbuf 0 true in
  let ctx = Unique_name_ctx.create_new_name_ctx 0 "lamia$" in
  let annot_block = Lamia_converter.convert_module ctx modl in
  let uid_block, annot_map = Lamia_converter.annot_to_uid annot_block in
  let abstract_block, stmt_map = Analysis_lift_ast.lift_block_top uid_block in
  let analysis = construct_pds abstract_block in
  analysis, annot_map, stmt_map
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

let get_starting_state annot_map stmt_map line =
  (* TODO: Reverse uid map to get uid to start at *) ignore annot_map; ignore line;
  (* TODO: Get the appropriate stmt *) ignore stmt_map;
  Program_state.End
;;

let make_query analysis_result target_str =
  match String.get target_str 0 with
  | '&' ->
    lookup_memory Program_state.End (Memory_variable(target_str)) analysis_result
  | _ ->
    lookup_value Program_state.End (Value_variable(target_str)) analysis_result
;;

let main () =
  let prog = get_python_prog () in
  let analysis_result, annot_map, stmt_map = parse_and_analyze prog in
  let rec loop analysis_result =
    let target_str, line = get_user_query () in
    let starting_state = get_starting_state annot_map stmt_map line in
    let results, analysis_result =
      lookup_value starting_state (Value_variable(target_str)) analysis_result
    in
    print_endline @@ "The possible values of " ^ target_str ^ " are " ^
                     Jhupllib_pp_utils.pp_to_string (Jhupllib_pp_utils.pp_list pp_value) (List.of_enum results)
    ;
    loop analysis_result
  in
  loop analysis_result
;;

main ();;
