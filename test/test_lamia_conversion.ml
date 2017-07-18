open OUnit2;;
open Batteries;;
open Jhupllib;;

open Python2_ast_pipeline;;

open Lamia_converter;;
open Lamia_ast_pretty;;

let parse_to_normalized_safe prog short_names =
  try
    parse_to_normalized prog short_names
  with
  | Python2_parser.Parse_error p ->
    let open Lexing in
    assert_failure (Printf.sprintf "Error in line %d, col %d."
                      p.pos_lnum p.pos_cnum)
;;

let lamia_to_string prog = Pp_utils.pp_to_string pp_block_top prog;;

let gen_module_test (name : string) (prog : string)
    (expected : string) =
  name>::
  ( fun _ ->
      let actual = parse_to_normalized_safe prog 1 true in
      let ctx = Unique_name_ctx.create_new_name_ctx 0 "lamia$" in
      let lamia_prog = convert_module ctx actual in
      let lamia_str = lamia_to_string lamia_prog in

      assert_equal ~printer:(fun x -> x) ~cmp:String.equal expected lamia_str;
  )
;;

let tests =
  "test_lamia_converter">:::
  [
    gen_module_test "int_add_test" "1+2" ""
  ]
