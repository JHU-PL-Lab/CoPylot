open Batteries;;
open Analysis_lift_ast;;
open Analysis_construct_cfg;;
open Analysis_lookup;;
open Analysis_grammar;;
open Analysis_types;;

let parse_and_analyze lexbuf =
  let block = Lamia_parser.parse_from_lexbuf lexbuf in
  construct_pds @@ lift_block block
;;

let make_query analysis_result target_str =
  match String.get target_str 0 with
  | '&' ->
    lookup_memory Program_state.End (Memory_variable(target_str)) analysis_result
  | _ ->
    lookup_value Program_state.End (Value_variable(target_str)) analysis_result
;;

let main () =
  let buf = Lexing.from_channel stdin in
  print_endline "Enter a lamia program:";
  print_endline "-------------";
  let analysis_result = parse_and_analyze buf in
  let rec loop analysis_result =
    print_endline "Enter a lamia variable name:";
    print_endline "-------------";
    let target_str = input_line stdin in
    if String.length target_str > 0 then
      let results, analysis_result = make_query analysis_result target_str in
      print_endline @@ "The possible values of " ^ target_str ^ " are " ^
                       Jhupllib_pp_utils.pp_to_string (Jhupllib_pp_utils.pp_list pp_value) (List.of_enum results)
      ;
      loop analysis_result
  in
  loop analysis_result
;;

main ();;
