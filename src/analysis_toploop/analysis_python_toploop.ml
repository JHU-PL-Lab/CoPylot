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
  let abstract_block, _ = Analysis_lift_ast.lift_block_top uid_block in
  let analysis = construct_pds abstract_block in
  analysis, abstract_block, uid_ctx
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

let get_starting_state prog uid_ctx line =
  let Block(stmts) = prog in
  (* We want the annot nearest the input line number but not before it *)
  (* TODO: We can do this faster by using the stmt_map returned from lift_block_top *)
  let rec find_first_valid stmts =
    match stmts with
    | [] -> None
    | stmt::rest ->
      let Statement(u, d) = stmt in
      let annot = Uid_ctx.get_annotation_from_uid uid_ctx u in
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

let make_query analysis_result target_str starting_state =
  let results, new_analysis =
    lookup_value starting_state (Value_variable("scope")) analysis_result
  in
  let possible_values =
    ignore target_str;
    Enum.map
      (fun v -> v) (*TODO: Look for target_str in the scope bindings*)
      results
  in
  possible_values, new_analysis
;;

let main () =
  let prog = get_python_prog () in
  let analysis_result, lamia_prog, uid_ctx = parse_and_analyze prog in
  let rec loop analysis_result =
    let target_str, line = get_user_query () in
    let starting_state = get_starting_state lamia_prog uid_ctx (int_of_string line) in
    let results, analysis_result =
      make_query analysis_result target_str starting_state
    in
    print_endline @@ "The possible values of " ^ target_str ^ " are " ^
                     Jhupllib_pp_utils.pp_to_string (Jhupllib_pp_utils.pp_list pp_value) (List.of_enum results)
    ;
    loop analysis_result
  in
  loop analysis_result
;;

main ();;
