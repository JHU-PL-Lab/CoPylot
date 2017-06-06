open OUnit2
open Batteries
open Jhupllib
open Python2_parser
open Lexing
module Simplified = Python2_simplified_ast
open Python2_normalized_ast
module Lift = Python2_ast_lifter
module Simplify = Python2_ast_simplifier
module Normalize = Python2_ast_normalizer
open Uid_generation
open Python2_analysis

let string_of_modl m = Pp_utils.pp_to_string pp_modl m;;
let equivalent_modl m1 m2 = equal_modl m1 m2;;

let parse_from_string_safe str =
  try
    parse_from_string str
  with
  | Python2_parser.Parse_error p ->
    assert_failure (Printf.sprintf "Error in line %d, col %d."
                      p.pos_lnum p.pos_cnum)
;;

(* Functions to hide testing boilerplate *)

let gen_analysis_test (name : string) (prog : string)
    (test_func : stmt list -> bool) =
  name>::
  ( fun _ ->
      let concrete = parse_from_string_safe (prog ^ "\n") in
      let abstract = Lift.lift_modl concrete in
      let simplified =
        (* Occasionally a test will fail; resetting here might help *)
        Simplify.reset_unique_name ();
        Simplify.simplify_modl abstract in
      Simplify.reset_unique_name ();
      let ctx = create_new_uid_context () in
      let actual = Normalize.normalize_modl ctx simplified in
      Normalize.reset_unique_name ();
      match actual with
      | Module (body, _) ->
        assert_bool ("Incorrect Tree:\n" ^ string_of_modl actual)
          (test_func body)
  )
;;

let expect_error_test
    (name : string)
    (prog : string)
    (expected : exn) =
  name>::
  (fun _ ->
     let concrete = parse_from_string_safe (prog ^ "\n") in
     let abstract = Lift.lift_modl concrete in
     let simplified = Simplify.simplify_modl abstract in
     let ctx = create_new_uid_context () in
     Simplify.reset_unique_name ();
     assert_raises
       expected
       (fun _ ->
          Normalize.normalize_modl ctx simplified)
  )
;;

let first_test =
  "first_test">::
  ( fun _ ->
      let actual = Module([Assign("x", SimpleExpr(Literal(Num(Int(Zero), 1, None), 2, None), 3, None), 4, None);
                           Assign("y", SimpleExpr(Literal(Num(Int(Pos), 5, None), 6, None), 7, None), 8, None);], 0) in
      let open Python2_pds in
      assert_equal ~printer:dump (Python2_pds.Answer_set.singleton (Num(Int(Zero)))) (analyze_uid actual 8 "x")
  )

let test_lex_cfg =
  "test_cfg">::
  ( fun _ ->
      let line1 = Assign("x", SimpleExpr(Literal(Num(Int(Zero), 1, None), 2, None), 3, None), 4, None) in
      let line2 = Assign("y", SimpleExpr(Literal(Num(Int(Pos), 5, None), 6, None), 7, None), 8, None) in
      let actual = Module([line1;
                           line2;], 0) in
      let Python2_cfg.Cfg.Cfg(lex, _) = Python2_cfg.Cfg.create actual in
      let open Python2_cfg.Lexical_cfg in let open Python2_cfg in
      assert_equal (empty
                    |> add_edge (Edge(Start, Program_point(line1)))
                    |> add_edge (Edge(Program_point(line1), Program_point(line2)))
                    |> add_edge (Edge(Program_point(line2), End))
                   ) lex
  )

let test_ctrl_cfg =
  "test_ctrl_cfg">::
  ( fun _ ->
      let line1 = Assign("x", SimpleExpr(Literal(Num(Int(Zero), 1, None), 2, None), 3, None), 4, None) in
      let line2 = Assign("y", SimpleExpr(Literal(Num(Int(Pos), 5, None), 6, None), 7, None), 8, None) in
      let actual = Module([line1;
                           line2;], 0) in
      let analysis = Analysis_result.create actual in
      let open Python2_analysis_result_structure in
      let Python2_cfg.Cfg.Cfg(_, ctrl) = analysis.analysis_cfg in
      let open Python2_cfg.Control_cfg in let open Python2_cfg in
      assert_equal ~printer:dump
        (empty
         |> add_edge (Edge(Start, Program_point(line1)))
         |> add_edge (Edge(Program_point(line1), Program_point(line2)))
         |> add_edge (Edge(Program_point(line2), End))
        ) ctrl
  )

let str_test =
  "str_test">::
  ( fun _ ->
      let actual = Module([Assign("x", SimpleExpr(Literal(Str(StringLiteral("a"), 1, None), 2, None), 3, None), 4, None);
                           Assign("y", SimpleExpr(Literal(Num(Int(Pos), 5, None), 6, None), 7, None), 8, None);], 0) in
      let open Python2_pds in
      assert_equal ~printer:dump (Python2_pds.Answer_set.singleton (Str(StringLiteral("a")))) (analyze_end actual "x")
  )

let bool_test =
  "bool_test">::
  ( fun _ ->
      let actual = Module([Assign("x", SimpleExpr(Literal(Bool(false, 1, None), 2, None), 3, None), 4, None);
                           Assign("y", SimpleExpr(Literal(Num(Int(Pos), 5, None), 6, None), 7, None), 8, None);], 0) in
      let open Python2_pds in
      assert_equal ~printer:dump (Python2_pds.Answer_set.singleton (Bool(false))) (analyze_uid actual 8 "x")
  )

let reassign_test =
  "reassign_test">::
  ( fun _ ->
      let actual = Module([Assign("x", SimpleExpr(Literal(Bool(true, 1, None), 2, None), 3, None), 4, None);
                           Assign("x", SimpleExpr(Literal(Bool(false, 5, None), 6, None), 7, None), 8, None);], 0) in
      let open Python2_pds in
      assert_equal ~printer:dump (Python2_pds.Answer_set.singleton (Bool(false))) (analyze_end actual "x")
  )

let reassign_midpoint_test =
  "reassign_midpoint_test">::
  ( fun _ ->
      let actual = Module([Assign("x", SimpleExpr(Literal(Bool(true, 1, None), 2, None), 3, None), 4, None);
                           Assign("x", SimpleExpr(Literal(Bool(false, 5, None), 6, None), 7, None), 8, None);], 0) in
      let open Python2_pds in
      assert_equal ~printer:dump (Python2_pds.Answer_set.singleton (Bool(true))) (analyze_uid actual 8 "x")
  )

let alias_test =
  "alias_test">::
  ( fun _ ->
      let actual = Module([Assign("x", SimpleExpr(Literal(Bool(true, 1, None), 2, None), 3, None), 4, None);
                           Assign("y", SimpleExpr(Name("x", 5, None), 6, None), 7, None);], 0) in
      let open Python2_pds in
      assert_equal ~printer:dump (Python2_pds.Answer_set.singleton (Bool(true))) (analyze_end actual "y")
  )

let tests =
  "analysis_ast">:::
  [
    first_test;
    test_lex_cfg;
    test_ctrl_cfg;
    str_test;
    bool_test;
    reassign_test;
    reassign_midpoint_test;
    alias_test;
  ]
