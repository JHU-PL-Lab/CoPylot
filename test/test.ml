open OUnit2;;

let all_tests =
  "tests" >:::
  [
    (* Test_parser.tests; *)
    (* Test_abstract_ast.tests; *)
    (* Test_intentional_failures.tests; *)
    Test_rename.tests
  ]
;;

run_test_tt_main all_tests;;
