open Batteries;;
open Analysis_lift_ast;;
open Analysis_construct_cfg;;
open Analysis_lookup;;
open Analysis_grammar;;
open Analysis_types;;

Jhupllib_logger_utils.set_default_logging_level `debug;;

let parse_and_analyze lexbuf =
  let block = Lamia_parser.parse_from_lexbuf lexbuf in
  let lifted_block, uid_map = lift_block_top block in
  fst @@ construct_analysis lifted_block, uid_map
;;

let make_query analysis_result uid_map uid_str target_str =
  let uid = int_of_string uid_str in
  let starting_point =
    try
      let s = Counter_hashtbl.Counter_hashtbl.find uid_map uid in
      Program_state.Stmt(s)
    with
    | Not_found -> Program_state.End
  in
  print_endline @@ "Beginning lookup from: " ^ (match starting_point with | Program_state.End -> "End" | _ -> string_of_int uid);
  match String.get target_str 0 with
  | '&' ->
    lookup_memory starting_point (Memory_variable(target_str)) analysis_result
  | _ ->
    lookup_value starting_point (Value_variable(target_str)) analysis_result
;;

let rec get_input () =
  let target_str = input_line stdin in
  if String.length target_str = 0 then
    get_input ()
  else
    target_str
;;

let main () =
  let buf = Lexing.from_channel stdin in
  print_endline "Enter a lamia program:";
  print_endline "-------------";
  let analysis_result, uid_map = parse_and_analyze buf in
  let rec loop analysis_result =
    print_endline "Enter a lamia variable name:";
    print_endline "-------------";
    let target_str = get_input () in
    print_endline "Enter a uid (if not found, we'll start at the end):";
    print_endline "-------------";
    let uid_str = get_input () in
    let results, analysis_result = make_query analysis_result uid_map uid_str target_str in
    print_endline @@ "The possible values of " ^ target_str ^ " are " ^
                     Jhupllib_pp_utils.pp_to_string (Jhupllib_pp_utils.pp_list pp_value) (List.of_enum results)
    ;
    loop analysis_result
  in
  loop analysis_result
;;

main ();;
