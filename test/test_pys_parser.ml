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
    "Int+"
    {uid= -1;
     exception_target=None;
     multi=false;
     body=Literal(Num(Int(Pos)))}
;;

let assign_test = gen_stmt_test "assign_test"
    "@1::F: a = Int+;"
    {uid=1;
     exception_target=None;
     multi=false;
     body=Assign("a",{uid= -2;
                      exception_target=None;
                      multi=false;
                      body=SimpleExpr(
                          {uid= -1;
                           exception_target=None;
                           multi=false;
                           body=Literal(Num(Int(Pos)))})})}
;;

let empty_funcdef_test = gen_stmt_test "empty_funcdef_test"
    "@1::F: def f() = {};"
    {uid=1;
     exception_target=None;
     multi=false;
     body=FunctionDef("f",
                      [],
                      [])}
;;

let funcdef_test = gen_stmt_test "funcdef_test"
    "@1::F: def f(a,b) = {@2::F: return @s 3::F: a;};"
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
    "@1::F: print Int+, a > \"a\";"
    {uid= 1;
     exception_target=None;
     multi=false;
     body=Print(Some{uid= -3;
                     exception_target=None;
                     multi=false;
                     body=Literal(Str(StringLiteral "a"))},
                [{uid= -1;
                  exception_target=None;
                  multi=false;
                  body=Literal(Num(Int(Pos)))};
                 {uid= -2;
                  exception_target=None;
                  multi=false;
                  body=Name("a")}
                ],
                false)}
;;

let goto_test = gen_module_test "goto_test"
    "@1::T: goto 2; @2::T: gotoifn @s 3::T: true 1;"
    [{ uid = 1;
       exception_target = None;
       multi = true;
       body = (Goto 2) };
     { uid = 2;
       exception_target = None;
       multi = true;
       body =GotoIfNot ({uid = 3;
                         exception_target = None;
                         multi = true;
                         body =(Literal(Bool true))},1) }
    ]
;;

let call_attribute_test = gen_module_test "call_attribute_test"
    "@1::F: var = f(a,b); @2::F: a = var.__add__; "
    [{ uid = 1; exception_target = None; multi = false;
       body =
         (Assign("var", { uid = -4;
                          exception_target = None;
                          multi = false;
                          body = (Call ( { uid = -1;
                                           exception_target = None;
                                           multi = false;
                                           body = (Name "f") },
                                         [{ uid = -2;
                                            exception_target = None;
                                            multi = false;
                                            body = (Name "a") };
                                          { uid = -3;
                                            exception_target = None;
                                            multi = false;
                                            body = (Name "b") }
                                         ]
                                       ))}))};
     { uid = 2;
       exception_target = None;
       multi = false;
       body =(Assign ("a",{ uid = -6;
                            exception_target = None;
                            multi = false;
                            body = (Attribute ({ uid = -5;
                                                 exception_target = None;
                                                 multi = false;
                                                 body = (Name "var") },
                                               "__add__"))}))}
    ]
;;

let list_tuple_test = gen_module_test "list_tuple_test"
    "@1::F: var = [Int+,Int+,Float-]; tpl = (Float0,Int0); "
    [{ uid = 1; exception_target = None; multi = false;
       body =
         (Assign ("var",{ uid = -4;
                          exception_target = None;
                          multi = false;
                          body = (List[{ uid = -1;
                                         exception_target = None;
                                         multi = false;
                                         body = (Literal(Num(Int Pos)))};
                                       { uid = -2;
                                         exception_target = None;
                                         multi = false;
                                         body = (Literal(Num(Int Pos)))};
                                       { uid = -3;
                                         exception_target = None;
                                         multi = false;
                                         body = (Literal(Num(Float Neg)))}
                                      ])}))};
     { uid = -8; exception_target = None;
       multi = false;
       body = (Assign ("tpl", { uid = -7;
                                exception_target = None;
                                multi = false;
                                body = (Tuple [{ uid = -5;
                                                 exception_target = None;
                                                 multi = false;
                                                 body = (Literal(Num(Float Zero)))};
                                               { uid = -6;
                                                 exception_target = None;
                                                 multi = false;
                                                 body = (Literal(Num(Int Zero)))}])}))}
    ]
;;

let builtin_test = gen_module_test "builtin_test"
    "a = type(Float+); def f(x) = {y = bool(x); return y; }; b = slice(Int0,Int+);"
    [{ uid = -16;
       exception_target = None;
       multi = false;
       body =(Assign ("a",{ uid = -3;
                            exception_target = None;
                            multi = false;
                            body =(Call ({ uid = -1;
                                           exception_target = None;
                                           multi = false;
                                           body =(Literal(Builtin Builtin_type))},
                                         [{ uid = -2;
                                            exception_target = None;
                                            multi = false;
                                            body = (Literal(Num(Float Pos)))}]))
                          }))};
     { uid = -15;
       exception_target = None;
       multi = false;
       body =(FunctionDef ("f", ["x"],
                           [{ uid = -9;
                              exception_target = None;
                              multi = false;
                              body =(Assign ("y",{ uid = -6;
                                                   exception_target = None;
                                                   multi = false;
                                                   body =(Call ({ uid = -4;
                                                                  exception_target = None;
                                                                  multi = false;
                                                                  body =(Literal(Builtin Builtin_bool))
                                                                },
                                                                [{ uid = -5;
                                                                   exception_target = None;
                                                                   multi = false;
                                                                   body = (Name "x") }]))}))
                            };
                            { uid = -8;
                              exception_target = None;
                              multi = false;
                              body =(Return(Some { uid = -7;
                                                   exception_target = None;
                                                   multi = false;
                                                   body = (Name "y") }))}
                           ]))};
     { uid = -14;
       exception_target = None;
       multi = false;
       body = (Assign ("b",{ uid = -13;
                             exception_target = None;
                             multi = false;
                             body =(Call ({ uid = -10;
                                            exception_target = None;
                                            multi = false;
                                            body =(Literal(Builtin Builtin_slice))},
                                          [{ uid = -11;
                                             exception_target = None;
                                             multi = false;
                                             body =
                                               (Literal(Num (Int Zero)))
                                           };
                                           { uid = -12;
                                             exception_target = None;
                                             multi = false;
                                             body = (Literal(Num (Int Pos)))}
                                          ]))
                           }))
     }]
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
    call_attribute_test;
    list_tuple_test;
    builtin_test;
  ]
