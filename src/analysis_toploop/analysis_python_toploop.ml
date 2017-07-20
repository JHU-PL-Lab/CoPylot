open Batteries;;
open Analysis_construct_cfg;;
open Analysis_lookup;;
open Analysis_grammar;;
open Analysis_types;;

let parse_and_analyze lexbuf =
  let modl = Python2_ast_pipeline.parse_to_normalized lexbuf 0 true in
  let ctx = Unique_name_ctx.create_new_name_ctx 0 "lamia$" in
  let annot_block = Lamia_converter.convert_module ctx modl in
  let uid_block, uid_ctx = Lamia_converter.annot_to_uid annot_block in
  let abstract_block, stmt_map = Analysis_lift_ast.lift_block_top uid_block in
  let analysis = construct_pds abstract_block in
  analysis, uid_ctx, stmt_map
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

let get_starting_state uid_ctx stmt_map line =
  let all_annots = Uid_ctx.Annot_hashtbl.keys uid_ctx.Uid_ctx.annot_map in
  (* We want the annot nearest the input line number but not under it *)
  let rec get_best_annot annots limit best_so_far =
    let next_annot = Enum.get annots in
    match next_annot with
    | None -> best_so_far
    | Some(a) ->
      let next_pos = Python2_ast.Pos.to_pos a in
      let next_best =
        match best_so_far with
        | None ->
          if next_pos.Lexing.pos_lnum > limit then
            next_annot
          else
            None
        | Some(best) ->
          let pos = Python2_ast.Pos.to_pos best in
          let best_lnum = pos.Lexing.pos_lnum in
          if next_pos.Lexing.pos_lnum > limit &&
             next_pos.Lexing.pos_lnum < best_lnum
          then
            next_annot
          else
            best_so_far
      in
      get_best_annot annots limit next_best
  in
  let best_annot = get_best_annot all_annots line None in
  match best_annot with
  | None -> Program_state.End
  | Some(best) ->
    let uids = Uid_ctx.get_uids_from_annot uid_ctx best in
    let min_uid = List.min uids in
    let starting_stmt = Counter_hashtbl.Counter_hashtbl.find stmt_map min_uid in
    Program_state.Stmt(starting_stmt)
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
  let analysis_result, uid_ctx, stmt_map = parse_and_analyze prog in
  let rec loop analysis_result =
    let target_str, line = get_user_query () in
    let starting_state = get_starting_state uid_ctx stmt_map (int_of_string line) in
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
