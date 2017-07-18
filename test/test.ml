open OUnit2;;

let all_tests =
  "tests" >:::
  [
    Test_parser.tests;
    Test_rename.tests;
    Test_simplified_ast.tests;
    (* Test_normalized_ast.tests; *)

    Test_lamia_parser.tests;
    (* Test_lamia_conversion.tests; *)
  ]
;;

run_test_tt_main all_tests;;
