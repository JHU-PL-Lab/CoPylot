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

let gen_analysis_test (name : string) (prog : string)
    (test_func : annotated_stmt list -> bool) =
  name>::
  ( fun _ ->
      let actual = parse_to_abstract_safe prog true in
      Python2_ast_simplifier.reset_unique_name ();
      Python2_ast_normalizer.reset_unique_name ();
      match actual with
      | Module (body, _) ->
        assert_bool ("Incorrect Tree:\n" ^ string_of_modl actual)
          (test_func body)
  )
;;

let annotate_stmt_full u ex m (s : stmt) : annotated_stmt =
  { uid = u; exception_target = ex; multi = m; body = s}
;;

let annotate_expr_full u ex m (e : expr) : annotated_expr =
  { uid = u; exception_target = ex; multi = m; body = e}
;;

let annotate_stmt u s =
  annotate_stmt_full u None false s
;;

let annotate_expr u e =
  annotate_expr_full u None false e
;;

let uid_test =
  "uid_test">::
  ( fun _ ->
      let actual = Module([annotate_stmt 1 @@ Assign("x", annotate_expr 2 @@ Literal(Num(Int(Zero))));
                           annotate_stmt 3 @@ Assign("y", annotate_expr 4 @@ Literal(Num(Int(Pos))));], 0) in
      let open Python2_normalization_ctx in
      let map = Python2_uid_stmt_map.get_uid_hashtbl actual in
      assert_equal (annotate_stmt 3 @@ Assign("y", annotate_expr 4 @@ Literal(Num(Int(Pos))))) (Uid_hashtbl.find map 3)
  )
;;

let test_lex_cfg =
  "test_lex_cfg">::
  ( fun _ ->
      let line1 = annotate_stmt 1 @@ Assign("x", annotate_expr 2 @@ Literal(Num(Int(Zero)))) in
      let line2 = annotate_stmt 3 @@ Assign("y", annotate_expr 4 @@ Literal(Num(Int(Pos)))); in
      let actual = Module([line1; line2;], 0) in
      let Python2_cfg.Cfg.Cfg(lex, _) = Python2_cfg.Cfg.create actual in
      let open Python2_cfg.Lexical_cfg in let open Python2_cfg in
      assert_equal
        (empty
         |> add_edge (Edge(Start, Program_point(line1)))
         |> add_edge (Edge(Program_point(line1), Program_point(line2)))
         |> add_edge (Edge(Program_point(line2), End))
        )
        lex
  )

let test_ctrl_cfg =
  "test_ctrl_cfg">::
  ( fun _ ->
      let line1 = annotate_stmt 1 @@ Assign("x", annotate_expr 2 @@ Literal(Num(Int(Zero)))) in
      let line2 = annotate_stmt 3 @@ Assign("y", annotate_expr 4 @@ Literal(Num(Int(Pos)))); in
      let actual = Module([line1; line2;], 0) in
      let analysis = Analysis_result.create actual in
      let open Python2_analysis_result_structure in
      let Python2_cfg.Cfg.Cfg(_, ctrl) = analysis.analysis_cfg in
      let open Python2_cfg.Control_cfg in let open Python2_cfg in
      assert_equal ~printer:dump
        (empty
         |> add_edge (Edge(Start, Program_point(line1)))
         |> add_edge (Edge(Program_point(line1), Program_point(line2)))
         |> add_edge (Edge(Program_point(line2), End))
        )
        ctrl
  )

let test_ctrl_cf2 =
  "test_ctrl_cfg">::
  ( fun _ ->
      let line1 = annotate_stmt 1 @@ Assign("$simplified_unique_name_0", annotate_expr 2 @@ Literal(Bool(true))) in
      let line2 = annotate_stmt 3 @@Assign("x", annotate_expr 4 @@ Name("$simplified_unique_name_0")) in
      let line3 = annotate_stmt 5 @@Assign("$simplified_unique_name_1", annotate_expr 6 @@ Literal(Num(Int(Neg)))) in
      let line4 = annotate_stmt 7 @@Assign("y", annotate_expr 8 @@ Name("$simplified_unique_name_1")) in
      let actual = Module([line1; line2; line3; line4;], 0) in
      let analysis = Analysis_result.create actual in
      let open Python2_analysis_result_structure in
      let Python2_cfg.Cfg.Cfg(_, ctrl) = analysis.analysis_cfg in
      let open Python2_cfg.Control_cfg in let open Python2_cfg in
      assert_equal ~printer:dump (* TODO: ~cmp = Cfg.equal *)
        (empty
         |> add_edge (Edge(Start, Program_point(line1)))
         |> add_edge (Edge(Program_point(line1), Program_point(line2)))
         |> add_edge (Edge(Program_point(line2), Program_point(line3)))
         |> add_edge (Edge(Program_point(line3), Program_point(line4)))
         |> add_edge (Edge(Program_point(line4), End))
        )
        ctrl;
      assert_equal ~printer:answer_set_to_string
        (let open Python2_pds in (Python2_pds.Answer_set.singleton (Bool(true))))
        (analyze_end actual "x")
  )

let int_test =
  "int_test">::
  ( fun _ ->
      let actual = Module([annotate_stmt 1 @@ Assign("x", annotate_expr 2 @@ Literal(Num(Int(Zero))))], 0) in
      let open Python2_pds in
      assert_equal ~printer:answer_set_to_string (Python2_pds.Answer_set.singleton (Num(Int(Zero)))) (analyze_end actual "x")
  )

let str_test =
  "str_test">::
  ( fun _ ->
      let actual = Module([annotate_stmt 1 @@ Assign("x", annotate_expr 2 @@ Literal(Str(StringLiteral("a"))))], 0) in
      let open Python2_pds in
      assert_equal ~printer:answer_set_to_string (Python2_pds.Answer_set.singleton (Str(StringLiteral("a")))) (analyze_end actual "x")
  )

let bool_test =
  "bool_test">::
  ( fun _ ->
      let actual = Module([annotate_stmt 1 @@ Assign("x", annotate_expr 2 @@ Literal(Bool(false)))], 0) in
      let open Python2_pds in
      assert_equal ~printer:answer_set_to_string (Python2_pds.Answer_set.singleton (Bool(false))) (analyze_end actual "x")
  )
;;
let undefined_test =
  "undefined_test">::
  ( fun _ ->
      let actual = Module([annotate_stmt 1 @@ Assign("x", annotate_expr 2 @@ Literal(Bool(false)))], 0) in
      let open Python2_pds in
      assert_equal ~printer:answer_set_to_string (Python2_pds.Answer_set.singleton (Undefined)) (analyze_end actual "y")
  )
;;
let skip_test =
  "skip_test">::
  ( fun _ ->
      let actual = Module([annotate_stmt 1 @@ Assign("x", annotate_expr 2 @@ Literal(Bool(true)));
                           annotate_stmt 3 @@ Assign("y", annotate_expr 4 @@ Literal(Bool(false)));], 0) in
      let open Python2_pds in
      assert_equal ~printer:answer_set_to_string (Python2_pds.Answer_set.singleton (Bool(true))) (analyze_end actual "x")
  )

let reassign_test =
  "reassign_test">::
  ( fun _ ->
      let actual = Module([annotate_stmt 1 @@ Assign("x", annotate_expr 2 @@ Literal(Bool(true)));
                           annotate_stmt 3 @@ Assign("x", annotate_expr 4 @@ Literal(Bool(false)));], 0) in
      let open Python2_pds in
      assert_equal ~printer:answer_set_to_string (Python2_pds.Answer_set.singleton (Bool(false))) (analyze_end actual "x")
  )

let reassign_midpoint_test =
  "reassign_midpoint_test">::
  ( fun _ ->
      let actual = Module([annotate_stmt 1 @@ Assign("x", annotate_expr 2 @@ Literal(Bool(true)));
                           annotate_stmt 3 @@ Assign("x", annotate_expr 4 @@ Literal(Bool(false)));], 0) in
      let open Python2_pds in
      assert_equal ~printer:answer_set_to_string (Python2_pds.Answer_set.singleton (Bool(true))) (analyze_uid 3 actual "x")
  )

let alias_test =
  "alias_test">::
  ( fun _ ->
      let actual = Module([annotate_stmt 1 @@ Assign("x", annotate_expr 2 @@ Literal(Bool(true)));
                           annotate_stmt 3 @@ Assign("y", annotate_expr 4 @@ Name("x"));], 0) in
      let open Python2_pds in
      assert_equal ~printer:answer_set_to_string (Python2_pds.Answer_set.singleton (Bool(true))) (analyze_end actual "y")
  )

let several_alias_test =
  "several_alias_test">::
  ( fun _ ->
      let actual = Module([annotate_stmt 1 @@ Assign("x", annotate_expr 2 @@ Literal(Bool(true)));
                           annotate_stmt 3 @@ Assign("y", annotate_expr 4 @@ Name("x"));
                           annotate_stmt 5 @@ Assign("z", annotate_expr 6 @@ Name("y"));], 0) in
      let open Python2_pds in
      assert_equal ~printer:answer_set_to_string (Python2_pds.Answer_set.singleton (Bool(true))) (analyze_end actual "z")
  )

let skip_and_alias_test =
  "skip_and_alias_test">::
  ( fun _ ->
      let actual = Module(
          [annotate_stmt 1 @@ Assign("$simplified_unique_name_0", annotate_expr 2 @@ Literal(Bool(true)));
           annotate_stmt 3 @@ Assign("x", annotate_expr 4 @@ Name("$simplified_unique_name_0"));
           annotate_stmt 5 @@ Assign("$simplified_unique_name_1", annotate_expr 6 @@ Literal(Num(Int(Neg))));
           annotate_stmt 7 @@ Assign("y", annotate_expr 8 @@ Name("$simplified_unique_name_1"));], 0) in
      let open Python2_pds in
      assert_equal ~printer:answer_set_to_string (Python2_pds.Answer_set.singleton (Bool(true))) (analyze_end actual "x")
  )

let tests =
  "test_analysis">:::
  [
    test_lex_cfg;
    test_ctrl_cfg;
    test_ctrl_cf2;
    int_test;
    str_test;
    bool_test;
    undefined_test;
    skip_test;
    reassign_test;
    reassign_midpoint_test;
    alias_test;
    several_alias_test;
    skip_and_alias_test;
  ]
