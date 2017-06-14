open OUnit2;;

let all_tests =
  "tests" >:::
  [
    Test_parser.tests;
    Test_abstract_ast.tests;
    Test_rename.tests;
    Test_simplified_ast.tests;
    Test_normalized_ast.tests;
    (* Test_analysis.tests; *)

    (* Test_normalized_pp.tests; *) (* TODO: Redundant with normalized_ast.tests *)
    Test_analysis_integration.tests;
    (* Test_pys_parser.tests; *)

  ]
;;

run_test_tt_main all_tests;;
