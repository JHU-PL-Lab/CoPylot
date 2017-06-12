open OUnit2
open Jhupllib
open Python2_normalized_ast
open Python2_pys_parser

let string_of_modl m = Pp_utils.pp_to_string pp_modl m;;

let equivalent_modl m1 m2 = equal_modl m1 m2;;

let gen_module_test (name : string) (pyssembly : string)
    (expected : annotated_stmt list)=
  name>::
  ( fun _ ->
      let actual = parse_from_string pyssembly in
      assert_equal ~printer:string_of_modl ~cmp:equivalent_modl
        (Module(expected, 0)) actual
  )
;;

let gen_stmt_test (name : string) (pyssembly : string)
    (expected : annotated_stmt)=
  name>::
  ( fun _ ->
      let actual = parse_stmt_from_string pyssembly in
      assert_equal ~printer:string_of_modl ~cmp:equivalent_modl
        (Module([expected], 0)) (Module([actual], 0))
  )
;;

let gen_cexpr_test (name : string) (pyssembly : string)
    (expected : annotated_cexpr)=
  name>::
  ( fun _ ->
      let actual = parse_cexpr_from_string pyssembly in
      assert_equal ~printer:string_of_modl ~cmp:equivalent_modl
        (Module([{uid=1;
                  exception_target=None;
                  multi=false;
                  body=Assign("a",expected)}], 0))
        (Module([{uid=1;
                  exception_target=None;
                  multi=false;
                  body=Assign("a",actual)}], 0))
  )
;;

let gen_sexpr_test (name : string) (pyssembly : string)
    (expected : annotated_sexpr)=
  name>::
  ( fun _ ->
      let actual = parse_sexpr_from_string pyssembly in
      assert_equal ~printer:string_of_modl ~cmp:equivalent_modl
        (Module([{uid=1;
                  exception_target=None;
                  multi=false;
                  body=SimpleExprStmt(expected)}], 0))
        (Module([{uid=1;
                  exception_target=None;
                  multi=false;
                  body=SimpleExprStmt(actual)}], 0))
  )
;;

(* Test begins *)
let int_test = gen_sexpr_test "int_test"
    "1::F: int pos"
    {uid=1;
     exception_target=None;
     multi=false;
     body=Literal(Num(Int(Pos)))}
;;

let assign_test = gen_stmt_test "assign_test"
    "1::F: a = 2::F:3::F: int pos;"
    {uid=1;
     exception_target=None;
     multi=false;
     body=Assign("a",{uid=2;
                      exception_target=None;
                      multi=false;
                      body=SimpleExpr(
                          {uid=3;
                           exception_target=None;
                           multi=false;
                           body=Literal(Num(Int(Pos)))})})}
;;

let empty_funcdef_test = gen_stmt_test "empty_funcdef_test"
    "1::F: def f() = {};"
    {uid=1;
     exception_target=None;
     multi=false;
     body=FunctionDef("f",
                      [],
                      [])}
;;

let funcdef_test = gen_stmt_test "funcdef_test"
    "1::F: def f(a,b) = {2::F: return 3::F: a;};"
    {uid=1;
     exception_target=None;
     multi=false;
     body=FunctionDef("f",
                      ["a";"b"],
                      [{uid=2;
                        exception_target=None;
                        multi=false;
                        body=Return(
                            Some({uid=3;
                                  exception_target=None;
                                  multi=false;
                                  body=Name("a")}))}])}
;;

let print_test = gen_stmt_test "print_test"
    "1::F: print 2::F: int pos, 3::F: a > 4::F: \"a\" ;"
    {uid=1;
     exception_target=None;
     multi=false;
     body=Print(Some{uid=4;
                     exception_target=None;
                     multi=false;
                     body=Literal(Str(StringLiteral "a"))},
                [{uid=2;
                  exception_target=None;
                  multi=false;
                  body=Literal(Num(Int(Pos)))};
                 {uid=3;
                  exception_target=None;
                  multi=false;
                  body=Name("a")}
                ],
                false)}
;;

let goto_test = gen_module_test "goto_test"
    "1::T: goto 2; 2::T: gotoifn 3::T: true 1;"
    [{ uid = 1;
       exception_target = None;
       multi = true;
       body = (Python2_normalized_ast.Goto 2) };
     { uid = 2;
       exception_target = None;
       multi = true;
       body =GotoIfNot ({uid = 3;
                         exception_target = None;
                         multi = true;
                         body =(Literal(Bool true))},1) }
    ]
;;

let tests =
  "pys_parser">:::
  [
    int_test;
    assign_test;
    empty_funcdef_test;
    funcdef_test;
    print_test;
    goto_test;
  ]
