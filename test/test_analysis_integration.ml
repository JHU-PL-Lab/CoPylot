open OUnit2;;
open Batteries;;
open Jhupllib;;

open Python2_ast_types;;
open Python2_abstract_ast;;
open Python2_ast_pipeline;;

open Python2_analysis;;
module Answer_set = Python2_pds.Answer_set;;

let string_of_modl m = Pp_utils.pp_to_string pp_modl m;;
let answer_set_to_string s =
  Pp_utils.pp_to_string
    (Pp_utils.pp_set Python2_pds.pp_answer Answer_set.enum) s;;

let parse_to_abstract_safe prog short_names =
  try
    parse_to_abstract prog short_names
  with
  | Python2_parser.Parse_error p ->
    let open Lexing in
    assert_failure (Printf.sprintf "Error in line %d, col %d."
                      p.pos_lnum p.pos_cnum)
;;

(* Functions to hide testing boilerplate *)
(* TODO: The fact that we have two functions for this makes me sad *)
let gen_analysis_test_uid (name : string) (prog : string)
    (expected : Answer_set.t) (u : uid) (v : identifier) =
  name>::
  ( fun _ ->
      let abstract = parse_to_abstract_safe prog true in
      Python2_ast_simplifier.reset_unique_name ();
      Python2_ast_normalizer.reset_unique_name ();
      let actual = analyze_uid u abstract v in
      assert_equal ~printer:answer_set_to_string expected actual
  )
;;

let gen_analysis_test_end (name : string) (prog : string)
    (expected : Answer_set.t) (v : identifier) =
  name>::
  ( fun _ ->
      let abstract = parse_to_abstract_safe prog true in
      Python2_ast_simplifier.reset_unique_name ();
      Python2_ast_normalizer.reset_unique_name ();
      let actual = analyze_end abstract v in
      assert_equal ~printer:answer_set_to_string expected actual
  )
;;

let int_test = gen_analysis_test_end "int_test"
    "x = 0"
    (let open Python2_pds in (Python2_pds.Answer_set.singleton (Num(Int(Zero)))))
    "x"
;;

let str_test = gen_analysis_test_end "str_test"
    "x = 'foo'"
    (let open Python2_pds in (Python2_pds.Answer_set.singleton (Str(StringLiteral("foo")))))
    "x"
;;

let bool_test = gen_analysis_test_end "bool_test"
    "x = True"
    (let open Python2_pds in (Python2_pds.Answer_set.singleton (Bool(true))))
    "x"
;;

let reassign_test = gen_analysis_test_end "reassign_test"
    "\n\
     x = True\n\
     x = -1"
    (let open Python2_pds in (Python2_pds.Answer_set.singleton (Num(Int(Neg)))))
    "x"
;;

let skip_test = gen_analysis_test_end "skip_test"
    "\n\
     x = True\n\
     y = -1"
    (let open Python2_pds in (Python2_pds.Answer_set.singleton (Bool(true))))
    "x"
;;

let alias_test = gen_analysis_test_end "alias_test"
    "\n\
     y = 1\n\
     x = y"
    (let open Python2_pds in (Python2_pds.Answer_set.singleton (Num(Int(Pos)))))
    "x"
;;

(* let reassign_midpoint_test =
   "reassign_midpoint_test">::
   ( fun _ ->
      let actual = Module([Assign("x", SimpleExpr(Literal(Bool(true, 1, None), 2, None), 3, None), 4, None);
                           Assign("x", SimpleExpr(Literal(Bool(false, 5, None), 6, None), 7, None), 8, None);], 0) in
      let open Python2_pds in
      assert_equal ~printer:dump (Python2_pds.Answer_set.singleton (Bool(true))) (analyze_uid 8 actual "x")
   ) *)

let tests =
  "test_analysis_integration">:::
  [
    int_test;
    str_test;
    bool_test;
    skip_test;
    reassign_test;
    (* reassign_midpoint_test; *)
    alias_test;
  ]
