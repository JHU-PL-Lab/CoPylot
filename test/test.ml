open OUnit2;;

let all_tests =
  "tests" >:::
  [
    Test_parser.tests;
    Test_rename.tests;
    Test_simplified_ast.tests;
    Test_abstract_ast.tests;
    Test_normalized_ast.tests;

    Test_pys_parser.tests;
    Test_abs_parser.tests;
    (* Test_normalized_pp.tests; *) (* Redundant with test_normalized_ast *)

    Test_lamia_parser.tests;

    Test_pys_interpreter.tests;
    (* Test_analysis.tests; *)
    (* Test_analysis_integration.tests; *)

  ]
;;

run_test_tt_main all_tests;;
