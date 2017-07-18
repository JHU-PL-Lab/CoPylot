open OUnit2;;
open Batteries;;
open Jhupllib;;

open Python2_ast_pipeline;;

open Lamia_converter;;
open Lamia_heap;;
open Lamia_interpreter;;

let parse_to_normalized_safe prog short_names =
  try
    parse_to_normalized prog short_names
  with
  | Python2_parser.Parse_error p ->
    let open Lexing in
    assert_failure (Printf.sprintf "Error in line %d, col %d."
                      p.pos_lnum p.pos_cnum)
;;

let print_eval_result = function
  | Evaluated_successfully ->
    "Evaluation successful."
  | Evaluated_to_exception(Lamia_ast.Memory_variable(y)) ->
    Printf.sprintf "Raised exception: %s\n" y
  | Evaluation_error s ->
    Printf.sprintf "Evaluation error: %s\n" s
;;

let gen_module_test (name : string) (prog : string)
    (expected_result : evaluation_result) =
  name>::
  ( fun _ ->
      let actual = parse_to_normalized_safe prog 1 true in
      let ctx = Python2_simplification_ctx.create_new_simplification_ctx 0 "$lamia_" in
      let lamia_prog = convert_module ctx actual in
      let lamia_result, lamia_heap = evaluate lamia_prog in

      let printer r =
        "Result: " ^ print_eval_result r ^ "\n" ^
        "Prog: " ^ Pp_utils.pp_to_string (Lamia_ast.pp_block (fun _ _ -> ())) lamia_prog ^ "\n" ^
        "Heap: " ^ Heap.to_string lamia_heap ^ "\n"
      in

      (* assert_equal ~printer:print_eval_result ~cmp:equal_evaluation_result expected_result lamia_result; *)
      assert_equal ~printer:printer ~cmp:equal_evaluation_result expected_result lamia_result;
  )
;;

let tests =
  "test_lamia_converter">:::
  [
    gen_module_test "int_add_test" "x = 1" @@ Evaluated_successfully;
  ]
