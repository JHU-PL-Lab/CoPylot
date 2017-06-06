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
      assert_equal ~printer:dump (Python2_pds.Answer_set.singleton (Num(Int(Zero)))) (analyze actual 8 "x")
  )

let test_cfg =
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

let tests =
  "abstract_ast">:::
  [
    first_test;
    test_cfg;
  ]
