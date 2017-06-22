open OUnit2
open Jhupllib
open Python2_ast_types
open Python2_normalized_ast
open Python2_pys_parser

let string_of_modl m = Pp_utils.pp_to_string pp_modl m;;

let equivalent_modl m1 m2 = equal_modl m1 m2;;

let gen_module_test (name : string) (pyssembly : string)
    (expected : annotated_stmt list)=
  name>::
  ( fun _ ->
      let actual = parse_from_string pyssembly in
      Python2_pys_utils.reset_uid ();
      assert_equal ~printer:string_of_modl ~cmp:equivalent_modl
        (Module(expected, 0)) actual
  )
;;

let gen_stmt_test (name : string) (pyssembly : string)
    (expected : annotated_stmt)=
  name>::
  ( fun _ ->
      let actual = parse_stmt_from_string pyssembly in
      Python2_pys_utils.reset_uid ();
      assert_equal ~printer:string_of_modl ~cmp:equivalent_modl
        (Module([expected], 0)) (Module([actual], 0))
  )
;;

let gen_expr_test (name : string) (pyssembly : string)
    (expected : annotated_expr)=
  name>::
  ( fun _ ->
      let actual = parse_expr_from_string pyssembly in
      Python2_pys_utils.reset_uid ();
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

(* Test begins *)
let int_test = gen_expr_test "int_test"
    "5"
    {uid= -1;
     exception_target=None;
     multi=false;
     body=Literal(Num(Int(5)))}
;;

let assign_test = gen_stmt_test "assign_test"
    "@1::F: a = 7;"
    {uid=1;
     exception_target=None;
     multi=false;
     body=Assign("a",{uid= -1 ;
                      exception_target=None;
                      multi=false;
                      body=Literal(Num(Int(7)))})}
;;

let empty_funcdef_test = gen_expr_test "empty_funcdef_test"
    "@e1::F: def () {}"
    {uid=1;
     exception_target=None;
     multi=false;
     body=Literal(FunctionVal([], []))}
;;

let funcdef_test = gen_expr_test "funcdef_test"
    "@e1::F: def (a,b) {@2::F: return a;}"
    {uid=1;
     exception_target=None;
     multi=false;
     body=Literal(FunctionVal(["a";"b"],
                              [{uid=2;
                                exception_target=None;
                                multi=false;
                                body=Return("a")}]))}
;;

let print_test = gen_stmt_test "print_test"
    "@1::F: print x, a > a;"
    {uid= 1;
     exception_target=None;
     multi=false;
     body=Print(Some("a"),
                ["x"; "a"],
                false)}
;;

let goto_test = gen_module_test "goto_test"
    "@1::T: goto 2; @2::T: goto 1 ifnot x;"
    [{ uid = 1;
       exception_target = None;
       multi = true;
       body = (Goto 2) };
     { uid = 2;
       exception_target = None;
       multi = true;
       body = GotoIfNot ("x",1) }
    ]
;;

let call_attribute_test = gen_module_test "call_attribute_test"
    "@1::F: var = f(a,b); @2::F: a = var.__add__; "
    [{ uid = 1; exception_target = None; multi = false;
       body =
         (Assign("var", { uid = -1;
                          exception_target = None;
                          multi = false;
                          body = (Call ( "f",
                                         ["a"; "b"]
                                       ))}))};
     { uid = 2;
       exception_target = None;
       multi = false;
       body =(Assign ("a",{ uid = -2;
                            exception_target = None;
                            multi = false;
                            body = (Attribute ("var",
                                               "__add__"))}))}
    ]
;;

let list_tuple_test = gen_module_test "list_tuple_test"
    "@1::F: var = [x,y,z]; tpl = (p,q); "
    [{ uid = 1; exception_target = None; multi = false;
       body =
         (Assign ("var",{ uid = -1;
                          exception_target = None;
                          multi = false;
                          body = (List["x"; "y"; "z"])}))};
     { uid = -3; exception_target = None;
       multi = false;
       body = (Assign ("tpl", { uid = -2;
                                exception_target = None;
                                multi = false;
                                body = (Tuple ["p"; "q"])}))}
    ]
;;

let builtin_test = gen_module_test "builtin_test"
    "x = type; y = bool; z = y(a); w = slice;"
    [{ uid = -8;
       exception_target = None;
       multi = false;
       body = (Assign ("x",{ uid = -1;
                             exception_target = None;
                             multi = false;
                             body = Literal(Builtin(Builtin_type));}))};
     { uid = -7;
       exception_target = None;
       multi = false;
       body = (Assign ("y",{ uid = -2;
                             exception_target = None;
                             multi = false;
                             body = Literal(Builtin(Builtin_bool));}))};
     { uid = -6;
       exception_target = None;
       multi = false;
       body = Assign("z", {uid = -3;
                           exception_target = None;
                           multi = false;
                           body = Call("y", ["a"]);});};

     { uid = -5;
       exception_target = None;
       multi = false;
       body = (Assign ("w",{ uid = -4;
                             exception_target = None;
                             multi = false;
                             body = Literal(Builtin(Builtin_slice));}))};
    ]
;;

let tests =
  "test_pys_parser">:::
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
