open Jhupllib;;

open OUnit2;;

open Lamia_ast;;
open Lamia_parser;;

let make_value_expression_test
    (name : string) (code : string) (expected : uid value_expression) =
  name >::
  fun _ ->
    let actual = parse_value_expression_from_string code in
    assert_equal ~printer:(Pp_utils.pp_to_string (pp_value_expression pp_uid))
      expected actual
;;

let make_statement_test
    (name : string) (code : string) (expected : uid statement) =
  name >::
  fun _ ->
    let actual = parse_statement_from_string code in
    assert_equal ~printer:(Pp_utils.pp_to_string (pp_statement pp_uid))
      expected actual
;;

let make_block_test
    (name : string) (code : string) (expected : uid block) =
  name >::
  fun _ ->
    let actual = parse_from_string code in
    assert_equal ~printer:(Pp_utils.pp_to_string (pp_block pp_uid))
      expected actual
;;

let int_test = make_value_expression_test "int_test"
    "5"
    (Integer_literal 5)
;;

let assign_value_test = make_statement_test "assign_value_test"
    "@1:let x = 9"
    (Statement(1,Let_expression(Value_variable "x", Integer_literal 9)))
;;

let alias_memory_test = make_statement_test "alias_memory_test"
    "@10:let &y = &z"
    (Statement(10,Let_alias_memory(Memory_variable "&y", Memory_variable "&z")))
;;

let auto_uid_test = make_statement_test "auto_uid_test"
    "let x = 4"
    (Statement(-1,Let_expression(Value_variable "x", Integer_literal 4)))
;;

let function_definition_test =
  make_value_expression_test "function_definition_test"
    "def (a,b) { let c = &a is &b }"
    (Function_expression(
        [ Value_variable("a");
          Value_variable("b");
        ],
        Block([ Statement(-1,
                          Let_is(Value_variable("c"),
                                 Memory_variable("&a"),
                                 Memory_variable("&b"))
                         )
              ])
      ))
;;

let tests =
  "test_lamia_parser" >:::
  [
    int_test;
    assign_value_test;
    alias_memory_test;
    auto_uid_test;
  ]
;;
