open OUnit2;;
open Batteries;;
open Jhupllib;;

open Lamia_parser;;

open Analysis_types;;

module Value_ord =
struct
  type t = value
  [@@deriving eq, ord, show]
end
;;

module Answer_set = Set.Make(Value_ord);;

let make_query analysis target_str =
  let open Analysis_lookup in
  let open Analysis_grammar in
  let results, _ =
    match String.get target_str 0 with
    | '&' ->
      lookup_memory Program_state.End (Memory_variable(target_str)) analysis
    | _ ->
      lookup_value Program_state.End (Value_variable(target_str)) analysis
  in
  Answer_set.of_enum results
;;

let gen_lamia_test
    (name : string)
    (prog : string)
    (target : string)
    (expected : value list) =
  name >::
  (fun _ ->
     let parsed_prog = parse_from_string prog in
     let lifted = fst @@ Analysis_lift_ast.lift_block_top parsed_prog in
     let analysis = fst @@ Analysis_construct_cfg.construct_analysis lifted in
     let actual = make_query analysis target in
     let expected = Answer_set.of_list expected in
     assert_equal
       ~cmp:Answer_set.equal
       ~printer:(Pp_utils.pp_to_string (Pp_utils.pp_set pp_value Answer_set.enum))
       expected
       actual
  )
;;

let literal_tests =
  [
    gen_lamia_test "int_pos_test" "let x = 5;;" "x" [Integer_value Pos];
    gen_lamia_test "int_neg_test" "let x = -5;;" "x" [Integer_value Neg];
    gen_lamia_test "int_zero_test" "let x = 0;;" "x" [Integer_value Zero];
    gen_lamia_test "string_test" "let x = \"foo\";;" "x" [String_value (String_exact "foo")];
    gen_lamia_test "bool_true_test" "let x = True;;" "x" [Boolean_value true];
    gen_lamia_test "bool_false_test" "let x = False;;" "x" [Boolean_value false];
    gen_lamia_test "empty_binding_test" "let x = {};;" "x" [Object_value AbstractStringMap.empty];
    gen_lamia_test "none_test" "let x = None;;" "x" [None_value];
    (* TODO: Function value? *)
    (* TODO: List_value *)
  ]

let tests =
  "test_lamia_parser" >:::
  literal_tests @
  [

  ]
;;
