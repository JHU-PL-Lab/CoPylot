open OUnit2;;
open Batteries;;
open Jhupllib;;

(* open Python2_ast_types;; *)
open Python2_ast_pipeline;;

open Python2_pys_interpreter_types;;
open Python2_pys_interpreter;;

let string_of_program_state s = Pp_utils.pp_to_string pp_program_state s;;

let parse_to_normalized_safe prog short_names =
  try
    parse_to_normalized prog short_names
  with
  | Python2_parser.Parse_error p ->
    let open Lexing in
    assert_failure (Printf.sprintf "Error in line %d, col %d."
                      p.pos_lnum p.pos_cnum)
;;

let gen_module_test (name : string) (prog : string)
    (expected : program_state ) =
  name>::
  ( fun _ ->
      let actual = parse_to_normalized_safe prog true in
      Python2_ast_simplifier.reset_unique_name ();
      Python2_ast_normalizer.reset_unique_name ();

      let result_state = interpret_program actual in
      assert_equal ~printer:string_of_program_state ~cmp:equal_program_state expected result_state
  )
;;

let tests =
  "test_pys_interpreter">:::
  [

  ]
