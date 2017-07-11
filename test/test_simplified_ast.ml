open OUnit2;;
open Batteries;;
open Jhupllib;;

module Concrete = Python2_concrete_ast;;

open Python2_ast_types;;
open Python2_ast_pipeline;;
open Python2_simplified_ast;;

let annot = Python2_ast.Pos.of_pos Lexing.dummy_pos;;

let string_of_stmt e  = Pp_utils.pp_to_string
    (Pp_utils.pp_list (pp_stmt (fun _ _ -> ()))) e;;
let equivalent_stmt e1 e2 = List.eq (equal_stmt ( fun _ _ -> true)) e1 e2;;

let string_of_modl m = Pp_utils.pp_to_string
    (pp_modl (fun _ _ -> ())) m;;
let equivalent_modl m1 m2 = equal_modl ( fun _ _ -> true) m1 m2;;

let parse_to_simplified prog short_names =
  try
    parse_to_simplified prog short_names
  with
  | Python2_parser.Parse_error p ->
    let open Lexing in
    assert_failure (Printf.sprintf "Error in line %d, col %d."
                      p.pos_lnum p.pos_cnum)
;;

(* Functions to hide testing boilerplate *)

let gen_module_test (name : string) (prog : string) (expected : 'a stmt list) =
  name>::
  ( fun _ ->
      let actual = parse_to_simplified prog 0 false in
      assert_equal ~printer:string_of_modl ~cmp:equivalent_modl
        (Module(expected, annot)) actual
  )
;;

let gen_stmt_test (name : string) (prog : string) (expected : 'a expr) =
  gen_module_test name prog @@ [(Expr(expected, annot))]
;;

let literal_tests =
  [
    gen_stmt_test "int_test" "4" (Num(Int(4), annot));
    gen_stmt_test "none_test" "None" (Name("*None", annot));
    gen_stmt_test "float_test" "1.7" (Num(Float(1.7), annot));
    gen_stmt_test "float_zero_test" "0.0" (Num(Float(0.0), annot));
  ]
;;

let unop_test = gen_stmt_test "unop_test_1"
    "+4"
    (Call(
        Attribute(Num(Int(4), annot), "__pos__", annot),
        [],
        annot))
;;

let unop_not_test = gen_stmt_test "unop_not_test"
    "not x"
    (UnaryOp(Not,
             Call(Builtin(Builtin_bool, annot),
                  [Name("x", annot)],
                  annot),
             annot))
;;

let boolop_and_test = gen_module_test "boolop_and_test"
    "x and True and -5"
    [
      Assign ("$simplified_unique_name_0",
              Name ("x", annot), annot);
      If (
        Call (
          Builtin (Python2_ast_types.Builtin_bool,
                   annot),
          [Name ("$simplified_unique_name_0", annot)],
          annot),
        [Assign ("$simplified_unique_name_1",
                 Bool (true, annot), annot);
         If (
           Call (
             Builtin (
               Python2_ast_types.Builtin_bool, annot),
             [Name ("$simplified_unique_name_1",
                    annot)
             ],
             annot),
           [Assign ("$simplified_unique_name_2",
                    Num (Int (-5), annot),
                    annot)
           ],
           [Assign ("$simplified_unique_name_2",
                    Name ("$simplified_unique_name_1",
                          annot),
                    annot)
           ],
           annot);
         Assign ("$simplified_unique_name_3",
                 Name ("$simplified_unique_name_2", annot),
                 annot)
        ],
        [Assign ("$simplified_unique_name_3",
                 Name ("$simplified_unique_name_0", annot), annot)
        ],
        annot);
      Expr(Name ("$simplified_unique_name_3", annot), annot)
    ]
;;

let boolop_or_test = gen_module_test "boolop_or_test"
    "x or False or 0"
    [
      Assign ("$simplified_unique_name_0",
              Name ("x", annot), annot);
      If (
        Call (
          Builtin (Python2_ast_types.Builtin_bool,
                   annot),
          [Name ("$simplified_unique_name_0", annot)],
          annot),
        [Assign ("$simplified_unique_name_3",
                 Name ("$simplified_unique_name_0", annot), annot)
        ],
        [Assign ("$simplified_unique_name_1",
                 Bool (false, annot), annot);
         If (
           Call (
             Builtin (
               Python2_ast_types.Builtin_bool, annot),
             [Name ("$simplified_unique_name_1",
                    annot)
             ],
             annot),
           [Assign ("$simplified_unique_name_2",
                    Name ("$simplified_unique_name_1",
                          annot),
                    annot)
           ],
           [Assign ("$simplified_unique_name_2",
                    Num (Int 0, annot),
                    annot)
           ],
           annot);
         Assign ("$simplified_unique_name_3",
                 Name ("$simplified_unique_name_2", annot),
                 annot)
        ],
        annot);
      Expr(Name ("$simplified_unique_name_3", annot), annot)
    ]
;;

let operator_tests =
  [
    unop_test;
    unop_not_test;
    boolop_and_test;
    boolop_or_test;
  ]
;;

let var_assign_test = gen_module_test "var_assign_test"
    "x = 5"
    [
      Assign(
        "x",
        Num(Int(5), annot),
        annot
      )
    ]
;;

let var_double_assign_test = gen_module_test "var_double_assign_test"
    "x = y = 5"
    [
      Assign(
        "$simplified_unique_name_0",
        Num(Int(5), annot),
        annot
      );
      Assign(
        "x",
        Name("$simplified_unique_name_0", annot),
        annot
      );
      Assign(
        "y",
        Name("$simplified_unique_name_0", annot),
        annot
      )
    ]
;;

let assign_iterator obj num =
  Assign(
    "$simplified_unique_name_" ^ string_of_int num,
    Attribute(
      Call(
        Attribute(
          obj,
          "__iter__",
          annot
        ),
        [],
        annot
      ),
      "next",
      annot),
    annot
  )
;;

(* let var_assign_from_tuple_test = gen_module_test "var_assign_from_tuple_test"
    "i, j = (-1,0)"
    [
      Assign(
        "$simplified_unique_name_0",
        Tuple(
          [
            Num(Int(-1), annot);
            Num(Int(0), annot);
          ],
          annot),
        annot);

      assign_iterator (Name("$simplified_unique_name_0", annot)) 1;

      TryExcept(
        [
          Assign(
            "$simplified_unique_name_2",
            Call(Name("$simplified_unique_name_1", annot), [], annot),
            annot);
          Assign(
            "$simplified_unique_name_3",
            Call(Name("$simplified_unique_name_1", annot), [], annot),
            annot);
          TryExcept(
            [
              Expr(
                Call(Name("$simplified_unique_name_1", annot), [], annot),
                annot
              );
              Raise(
                Call(
                  Builtin(Builtin_ValueError, annot),
                  [Str(StringLiteral("too many values to unpack"),
                       annot)],
                  annot),
                annot);
            ],
            [ExceptHandler(
                Some(Name("StopIteration", annot)),
                None,
                [
                  Pass(annot)
                ],
                annot
              )
            ],
            annot
          )
        ],
        [ExceptHandler(
            Some(Name("StopIteration", annot)),
            None,
            [
              Raise(
                Call(
                  Builtin(Builtin_ValueError, annot),
                  [Str(StringLiteral("too few values to unpack"),
                       annot)],
                  annot),
                annot);
            ],
            annot
          )],
        annot);
      Assign(
        "$simplified_unique_name_4",
        Name("$simplified_unique_name_2", annot),
        annot);
      Assign(
        "i",
        Name("$simplified_unique_name_4", annot),
        annot);
      Assign(
        "$simplified_unique_name_5",
        Name("$simplified_unique_name_3", annot),
        annot);
      Assign(
        "j",
        Name("$simplified_unique_name_5", annot),
        annot);
    ]
   ;; *)

let assign_to_index_test = gen_module_test "assign_to_index_test"
    "list[1+2] = 3"
    [
      Expr(
        Call(
          Attribute(
            Name("list", annot),
            "__setitem__",
            annot),
          [
            Call(
              Attribute(Num(Int(1), annot), "__add__", annot),
              [Num(Int(2), annot)],
              annot);
            Num(Int(3), annot);
          ],
          annot
        ),
        annot
      )
    ]
;;

let assign_to_slice_test = gen_module_test "assign_to_slice_test"
    "list[1:2] = 3"
    [
      Expr(
        Call(
          Attribute(
            Name("list", annot),
            "__setitem__",
            annot),
          [
            Call(
              Builtin(Builtin_slice, annot),
              [
                Num(Int(1), annot);
                Num(Int(2), annot);
                Name("*None", annot);
              ],
              annot
            );
            Num(Int(3), annot);
          ],
          annot
        ),
        annot
      )
    ]
;;

let assign_to_attribute_test = gen_module_test "assign_to_attribute_test"
    "obj.member = 7"
    [
      Expr(
        Call(
          Attribute(
            Name("obj", annot),
            "__setattr__",
            annot
          ),
          [
            Str("member", annot);
            Num(Int(7), annot);
          ],
          annot
        ),
        annot
      )
    ]
;;

let var_aug_assign_test = gen_module_test "var_aug_assign_test"
    "x *= -5"
    [
      TryExcept (
        [Assign ("$simplified_unique_name_1",
                 Attribute (
                   Name ("x", annot), "__imul__", annot),
                 annot)
        ],
        "$simplified_unique_name_2",
        [
          If (
            Call(Builtin(Builtin_bool, annot),
                 [Binop (
                     Call (
                       Builtin (
                         Builtin_type, annot),
                       [Name ("$simplified_unique_name_2",
                              annot)
                       ],
                       annot),
                     Is,
                     Builtin (Builtin_AttributeError, annot),
                     annot)],
                 annot),
            [Assign ("$simplified_unique_name_1",
                     Attribute (
                       Name ("x", annot), "__mul__", annot),
                     annot)
            ],
            [Raise (
                Name ("$simplified_unique_name_2", annot),
                annot)
            ],
            annot)
        ],
        annot);
      Assign ("x",
              Call (
                Name ("$simplified_unique_name_1", annot),
                [Num (Int(-5), annot)], annot),
              annot)

    ]
;;

let assign_tests =
  [
    var_assign_test;
    var_double_assign_test;
    (* var_assign_from_tuple_test; *)
    assign_to_index_test;
    assign_to_slice_test;
    assign_to_attribute_test;
    var_aug_assign_test;
  ]
;;

let var_cmp_test = gen_module_test "var_cmp_test"
    "x <= 42"
    [
      Expr(
        Call(Attribute(Name("x", annot),
                       "__le__",
                       annot),
             [Num(Int(42), annot)],
             annot
            ),
        annot
      )
    ]
;;

let funcdef_test = gen_module_test "funcdef_test"
    "def test_function(arg1,arg2):\n\treturn arg1"
    [
      Assign("test_function",
             FunctionVal([
                 "test_function$1_arg1";
                 "test_function$1_arg2";
               ],
                 [ (* Body *)
                   Return(Name("test_function$1_arg1", annot), annot)
                 ],
                 annot),
             annot)
    ]
;;

let no_ret_test = gen_module_test "no_ret_test"
    "def func(arg):\n\tx = 5"
    [
      Assign("func",
             FunctionVal(["func$1_arg"],
                         [
                           Assign("func$1_x", Num(Int 5, annot), annot);
                           Return(Name("*None", annot), annot);
                         ],
                         annot),
             annot);
    ]
;;


let not_func_test = gen_module_test "not_func_test"
    "def notfunc(arg):\n\tif arg:\n\t\treturn False\n\telse:\n\t\treturn True"
    [
      Assign("notfunc",
             FunctionVal(["notfunc$1_arg"],
                         [
                           If(Call(Builtin(Builtin_bool, annot),
                                   [Name("notfunc$1_arg", annot)],
                                   annot),
                              [Return(Bool(false, annot), annot)],
                              [Return(Bool(true, annot), annot)],
                              annot);
                         ],
                         annot),
             annot);
    ]
;;

let call_test = gen_stmt_test "call_test"
    "func(1,x,'foo')"
    (Call(
        Name("func", annot),
        [
          Num(Int(1), annot);
          Name("x", annot);
          Str("foo", annot);
        ],
        annot))
;;

let attribute_test = gen_stmt_test "attribute_test"
    "obj.member_var"
    (Attribute(
        Name("obj", annot),
        "member_var",
        annot))
;;

let attribute_call_test = gen_stmt_test "attribute_test"
    "obj.member_func()"
    (Call(
        Attribute(
          Name("obj", annot),
          "member_func",
          annot),
        [],
        annot
      ))
;;

let if_test = gen_module_test "if_test"
    "if x > 2:\n\tx = 3\nelif x < 0: x = x*-1\nelse: pass"
    [If (
        Call(Builtin(Builtin_bool, annot),
             [Call(Attribute(Name ("x", annot),
                             "__gt__",
                             annot),
                   [Num(Int 2, annot)],
                   annot)],
             annot),
        [
          Assign ("x", Num(Int 3, annot), annot);
        ],
        [If (
            Call(Builtin(Builtin_bool, annot),
                 [Call(Attribute(Name ("x", annot),
                                 "__lt__",
                                 annot),
                       [Num(Int 0, annot)],
                       annot)],
                 annot),
            [
              Assign ("x",
                      Call (
                        Attribute (
                          Name ("x", annot), "__mul__",
                          annot),
                        [Num(Int (-1), annot)],
                        annot),
                      annot)
            ],
            [Pass annot],
            annot)
        ],
        annot)
    ]
;;

let tuple_test = gen_stmt_test "tuple_test"
    "(1,2,3,4)"
    (Tuple (
        [
          Num(Int(1), annot);
          Num(Int(2), annot);
          Num(Int(3), annot);
          Num(Int(4), annot);
        ],
        annot
      ))
;;

let while_test = gen_module_test "while_test"
    "while x < 9001:\n\tx = x+1"
    [
      Assign("$simplified_unique_name_0",
             Call(Builtin(Builtin_bool, annot),
                  [Call(
                      Attribute(Name("x",annot),
                                "__lt__",
                                annot),
                      [Num(Int(9001), annot)],
                      annot)],
                  annot),
             annot);

      While(
        "$simplified_unique_name_0",
        [
          Assign("x",
                 Call(Attribute(Name("x", annot), "__add__", annot),
                      [Num(Int(1), annot)],
                      annot),
                 annot);
          Assign("$simplified_unique_name_0",
                 Call(Builtin(Builtin_bool, annot),
                      [Call(
                          Attribute(Name("x",annot),
                                    "__lt__",
                                    annot),
                          [Num(Int(9001), annot)],
                          annot)],
                      annot),
                 annot);
        ],
        annot
      )
    ]
;;

let for_test = gen_module_test "for_test"
    "for i in list:\n\tf(i)"
    [
      assign_iterator (Name("list", annot)) 0;
      TryExcept(
        [
          Assign("$simplified_unique_name_2",
                 Call(Builtin(Builtin_bool, annot),
                      [Bool(true, annot)],
                      annot),
                 annot);
          While(
            "$simplified_unique_name_2",
            [
              Assign(
                "i",
                Call(Name("$simplified_unique_name_0", annot), [], annot),
                annot
              );
              Expr(Call(Name("f", annot), [Name("i", annot)], annot), annot);
              Assign("$simplified_unique_name_2",
                     Call(Builtin(Builtin_bool, annot),
                          [Bool(true, annot)],
                          annot),
                     annot);
            ],
            annot)
        ],
        "$simplified_unique_name_1",
        [
          If(Call(Builtin(Builtin_bool, annot),
                  [Binop(
                      Call(Builtin(Builtin_type, annot),
                           [Name("$simplified_unique_name_1", annot)],
                           annot),
                      Is,
                      Name("StopIteration", annot),
                      annot)],
                  annot),
             [Pass(annot)],
             [Raise(Name("$simplified_unique_name_1", annot), annot)],
             annot)
        ],
        annot
      )
    ]
;;

let break_test = gen_module_test "break_test"
    "while x < 9001:\n\tbreak"
    [
      Assign("$simplified_unique_name_0",
             Call(Builtin(Builtin_bool, annot),
                  [Call(
                      Attribute(Name("x",annot),
                                "__lt__",
                                annot),
                      [Num(Int(9001), annot)],
                      annot)],
                  annot),
             annot);

      While(
        "$simplified_unique_name_0",
        [
          Break(annot);
          Assign("$simplified_unique_name_0",
                 Call(Builtin(Builtin_bool, annot),
                      [Call(
                          Attribute(Name("x",annot),
                                    "__lt__",
                                    annot),
                          [Num(Int(9001), annot)],
                          annot)],
                      annot),
                 annot);
        ],
        annot
      )
    ]
;;

let continue_test = gen_module_test "continue_test"
    "while x < 9001:\n\tcontinue"
    [
      Assign("$simplified_unique_name_0",
             Call(Builtin(Builtin_bool, annot),
                  [Call(
                      Attribute(Name("x",annot),
                                "__lt__",
                                annot),
                      [Num(Int(9001), annot)],
                      annot)],
                  annot),
             annot);

      While(
        "$simplified_unique_name_0",
        [
          Continue(annot);
          Assign("$simplified_unique_name_0",
                 Call(Builtin(Builtin_bool, annot),
                      [Call(
                          Attribute(Name("x",annot),
                                    "__lt__",
                                    annot),
                          [Num(Int(9001), annot)],
                          annot)],
                      annot),
                 annot);
        ],
        annot
      )
    ]
;;

let loop_tests =
  [
    while_test;
    for_test;
    break_test;
    continue_test;
  ]
;;

let raise_test = gen_module_test "raise_test"
    "raise ValueError"
    [Raise(
        Name("ValueError", annot),
        annot)]
;;

let try_block =
  "try:" ^
  "\n\tx = 5" ^
  "\nexcept ValueError:" ^
  "\n\tret = 'Error'" ^
  "\nexcept StopIteration as e:" ^
  "\n\tret = 'Other Error'" ^
  "\n"
;;

let try_test = gen_module_test "try_test"
    try_block
    [
      TryExcept(
        [
          Assign(
            "x",
            Num(Int(5), annot),
            annot)
        ],
        "$simplified_unique_name_0",
        [
          If (
            Call(Builtin(Builtin_bool, annot),
                 [Binop (
                     Call (
                       Builtin (
                         Builtin_type, annot),
                       [Name ("$simplified_unique_name_0",
                              annot)
                       ],
                       annot),
                     Is,
                     Name ("ValueError", annot),
                     annot)],
                 annot),
            [Assign ("ret",
                     Str ("Error", annot), annot)
            ],
            [
              If (
                Call(Builtin(Builtin_bool, annot),
                     [Binop (
                         Call (
                           Builtin (
                             Builtin_type, annot),
                           [Name (
                               "$simplified_unique_name_0", annot)
                           ],
                           annot),
                         Is,
                         Name ("StopIteration", annot),
                         annot)],
                     annot),
                [Assign ("e",
                         Name (
                           "$simplified_unique_name_0", annot),
                         annot);
                 Assign ("ret",
                         Str ("Other Error", annot), annot)
                ],
                [Raise (
                    Name (
                      "$simplified_unique_name_0", annot),
                    annot)
                ],
                annot)
            ],
            annot)

        ],
        annot)
    ]
;;

let triangle_def =
  "def triangle(n):" ^
  "\n\tcount = 0" ^
  "\n\ti=0" ^
  "\n\twhile count < n:" ^
  "\n\t\ti += count" ^
  "\n\t\tcount = count + 1" ^
  "\n\treturn i" ^
  "\n"
;;

let triangle_ast =
  Assign(
    "triangle",
    FunctionVal(
      ["triangle$1_n"], (* This test is currently failing: Change this to ["n"] to make it succeed, but this is a bug *)
      [
        Assign ("triangle$1_count",
                Num (Int 0, annot),
                annot);
        Assign ("triangle$1_i",
                Num (Int 0, annot),
                annot);

        Assign("$simplified_unique_name_0",
               Call(Builtin(Builtin_bool, annot),
                    [Call (Attribute(Name ("triangle$1_count", annot),
                                     "__lt__",
                                     annot),
                           [Name ("triangle$1_n", annot)],
                           annot)],
                    annot),
               annot);

        While(
          "$simplified_unique_name_0",
          [TryExcept (
              [Assign (
                  "$simplified_unique_name_2",
                  Attribute (
                    Name ("triangle$1_i", annot), "__iadd__",
                    annot),
                  annot);
              ],
              "$simplified_unique_name_3",
              [
                If (
                  Call(Builtin(Builtin_bool, annot),
                       [Binop (
                           Call (
                             Builtin (
                               Builtin_type, annot),
                             [Name ("$simplified_unique_name_3",
                                    annot)
                             ],
                             annot),
                           Is,
                           Builtin (Builtin_AttributeError, annot),
                           annot)],
                       annot),
                  [Assign ("$simplified_unique_name_2",
                           Attribute (
                             Name ("triangle$1_i", annot), "__add__", annot),
                           annot)
                  ],
                  [Raise (
                      Name ("$simplified_unique_name_3", annot),
                      annot)
                  ],
                  annot)
              ],
              annot);
           Assign ("triangle$1_i",
                   Call(Name("$simplified_unique_name_2",
                             annot),
                        [Name ("triangle$1_count", annot)], annot),
                   annot);
           Assign ("triangle$1_count",
                   Call (
                     Attribute (
                       Name ("triangle$1_count", annot),
                       "__add__", annot),
                     [Num (Int 1, annot)],
                     annot),
                   annot);
           Assign("$simplified_unique_name_0",
                  Call(Builtin(Builtin_bool, annot),
                       [Call (Attribute(Name ("triangle$1_count", annot),
                                        "__lt__",
                                        annot),
                              [Name ("triangle$1_n", annot)],
                              annot)],
                       annot),
                  annot);
          ],
          annot);
        Return (Name("triangle$1_i", annot), annot);
      ],
      annot),
    annot)
;;

let big_test = gen_module_test "big_test"
    (triangle_def ^ "\n[triangle(1),triangle(7)]")
    [
      triangle_ast;
      Expr(
        List(
          [
            Call(
              Name("triangle", annot),
              [Num(Int(1), annot)],
              annot
            );
            Call(
              Name("triangle", annot),
              [Num(Int(7), annot)],
              annot
            );
          ],
          annot
        ),
        annot)
    ]
;;

(* Tests of lists and slicing *)
let list_str = "[1,2,3,'four','five',2+4]";;
let list_expr =
  List(
    [
      Num(Int(1),annot);
      Num(Int(2),annot);
      Num(Int(3),annot);
      Str("four", annot);
      Str("five", annot);
      Call(Attribute(Num(Int(2),annot), "__add__", annot),
           [Num(Int(4), annot)],
           annot);
    ],
    annot
  )

let list_test = gen_stmt_test "list_test"
    list_str
    list_expr;;

let list_in_test = gen_stmt_test "lst_in_test"
    ("5 in " ^ list_str)
    (Call(Attribute(Num(Int(5), annot),
                    "__contains__",
                    annot),
          [list_expr],
          annot
         )
    )
;;

let gen_slice_test (name : string) (slice : string) (expected_slice: 'a expr) =
  gen_stmt_test
    name
    (list_str ^ slice)
    (Call(
        Attribute(list_expr,
                  "__getitem__",
                  annot),
        [ expected_slice ],
        annot))
;;

let list_tests =
  [
    list_test;
    list_in_test;
    (gen_slice_test "slice_test_1" "[0]"
       (Num(Int(0),annot)));
    (gen_slice_test "slice_test2" "[5-2]"
       (Call(Attribute(Num(Int(5), annot), "__sub__", annot),
             [Num(Int(2), annot)], annot)));
    (gen_slice_test "slice_test3" "[2:]"
       (Call(Builtin(Builtin_slice, annot),
             [
               Num(Int(2), annot);
               Name("*None", annot);
               Name("*None", annot);
             ],
             annot)));
    (gen_slice_test "slice_test4" "[:4]"
       (Call(Builtin(Builtin_slice, annot),
             [
               Name("*None", annot);
               Num(Int(4), annot);
               Name("*None", annot);
             ],
             annot)));
    (gen_slice_test "slice_test5" "[::3]"
       (Call(Builtin(Builtin_slice, annot),
             [
               Name("*None", annot);
               Name("*None", annot);
               Num(Int(3), annot);
             ],
             annot)));
    (gen_slice_test "slice_test6" "[2:4]"
       (Call(Builtin(Builtin_slice, annot),
             [
               Num(Int(2), annot);
               Num(Int(4), annot);
               Name("*None", annot);
             ],
             annot)));
    (gen_slice_test "slice_test7" "[2:4:-1]"
       (Call(Builtin(Builtin_slice, annot),
             [
               Num(Int(2), annot);
               Num(Int(4), annot);
               Num(Int(-1), annot);
             ],
             annot)));
  ]

(* Tests of various binary operations *)
let gen_binop_test (name : string) (prog : string) (lhs : 'a expr) (rhs : 'a expr) op =
  gen_stmt_test name prog (Call(Attribute(lhs, op, annot), [rhs], annot))
;;

let binop_tests =
  [
    (gen_binop_test "add_int_test" "42 + 9001"
       (Num(Int(42), annot)) (Num(Int(9001), annot)) "__add__");
    (gen_binop_test "add_float_test" "42.0 + 9001.75"
       (Num(Float(42.0), annot)) (Num(Float(9001.75), annot)) "__add__");
    (gen_binop_test "add_int_float_test" "42 + -9001.5"
       (Num(Int(42), annot)) (Num(Float(-9001.5), annot)) "__add__");

    (gen_binop_test "add_str_test" "'foo' + 'bar'"
       (Str("foo", annot)) (Str("bar", annot)) "__add__");
    (gen_binop_test "add_int_str_test" "42 + 'foo'"
       (Num(Int(42), annot)) (Str("foo", annot)) "__add__");

    (gen_binop_test "sub_int_test" "42 - 9001"
       (Num(Int(42), annot)) (Num(Int(9001), annot)) "__sub__");
    (gen_binop_test "mult_int_test" "42 * 9001"
       (Num(Int(42), annot)) (Num(Int(9001), annot)) "__mul__");
    (gen_binop_test "div_int_test" "42 / 9001"
       (Num(Int(42), annot)) (Num(Int(9001), annot)) "__div__");
    (gen_binop_test "mod_int_test" "42 % 9001"
       (Num(Int(42), annot)) (Num(Int(9001), annot)) "__mod__");
    (gen_binop_test "pow_int_test" "42 ** 9001"
       (Num(Int(42), annot)) (Num(Int(9001), annot)) "__pow__");

    (gen_binop_test "triple_binop_test" "(42 - 9001) + 17"
       (Call(Attribute(Num(Int(42), annot), "__sub__", annot),
             [Num(Int(9001), annot)],
             annot))
       (Num(Int(17), annot))
       "__add__");

    (gen_binop_test "order_of_operations_test" "1+2*3"
       (Num(Int(1), annot))
       (Call(Attribute(Num(Int(2), annot), "__mul__", annot),
             [Num(Int(3), annot)],
             annot))
       "__add__");
  ]

let expect_error_test
    (name : string)
    (prog : 'a Concrete.stmt list)
    (expected : exn) =
  name>::
  (fun _ ->
     assert_raises
       expected
       (fun _ ->
          let ctx = Python2_simplification_ctx.create_new_simplification_ctx 0 "$simp" in
          (List.concat (List.map (Python2_ast_simplifier.simplify_stmt ctx) prog)))
  )
;;

(* This is useful when creating tests for expect_error_test *)
let dummy_expr = Concrete.Num(Int(0), annot);;

let gen_some_concrete_assignment target =
  Concrete.Assign(
    [target],
    dummy_expr,
    annot)
;;

let bad_funcdef_test = expect_error_test "bad_funcdef_test"
    [
      Concrete.FunctionDef(
        "func",
        ([Concrete.Num(Int(5), annot)],
         None,
         None,
         []),
        [Concrete.Pass(annot)],
        [],
        annot
      )
    ]
    (Failure("The arguments in a function definition must be identifiers"))
;;

let bad_exception_handler_test = expect_error_test
    "bad_exception_handler_test"
    [Concrete.TryExcept(
        [],
        [
          Concrete.ExceptHandler(
            Some(Concrete.Name("foo", Concrete.Load, annot)),
            Some(dummy_expr),
            [],
            annot
          )
        ],
        [],
        annot
      )]
    (Failure("Second argument to exception handler must be an identifier"))
;;

let error_tests =
  let module Simplify = Python2_ast_simplifier in
  [
    expect_error_test "assign_to_num"
      [(gen_some_concrete_assignment
          (Concrete.Num(Int(0), annot)))]
      (Simplify.Invalid_assignment("can't assign to literal"));
    expect_error_test "assign_to_str"
      [(gen_some_concrete_assignment
          (Concrete.Str("", annot)))]
      (Simplify.Invalid_assignment("can't assign to literal"));
    expect_error_test "assign_to_bool"
      [(gen_some_concrete_assignment
          (Concrete.Bool(true, annot)))]
      (Simplify.Invalid_assignment("can't assign to literal"));
    expect_error_test "assign_to_boolop"
      [(gen_some_concrete_assignment
          (Concrete.BoolOp(Concrete.And, [dummy_expr; dummy_expr], annot)))]
      (Simplify.Invalid_assignment("can't assign to operator"));
    expect_error_test "assign_to_binop"
      [(gen_some_concrete_assignment
          (Concrete.BinOp(dummy_expr, Concrete.Add, dummy_expr, annot)))]
      (Simplify.Invalid_assignment("can't assign to operator"));
    expect_error_test "assign_to_unaryop"
      [(gen_some_concrete_assignment
          (Concrete.UnaryOp(Concrete.UAdd, dummy_expr, annot)))]
      (Simplify.Invalid_assignment("can't assign to operator"));
    expect_error_test "assign_to_ifexp"
      [(gen_some_concrete_assignment
          (Concrete.IfExp(dummy_expr, dummy_expr, dummy_expr, annot)))]
      (Simplify.Invalid_assignment("can't assign to conditional expression"));
    expect_error_test "assign_to_compare"
      [(gen_some_concrete_assignment
          (Concrete.Compare(dummy_expr, [Concrete.Lt], [dummy_expr], annot)))]
      (Simplify.Invalid_assignment("can't assign to comparison"));
    expect_error_test "assign_to_call"
      [(gen_some_concrete_assignment
          (Concrete.Call(dummy_expr, [], [], None, None, annot)))]
      (Simplify.Invalid_assignment("can't assign to function call"));
    bad_funcdef_test;
    bad_exception_handler_test;
  ]
;;

(* Run the tests *)
let tests =
  "Test_simplified_ast">:::
  literal_tests @
  operator_tests @
  binop_tests @
  list_tests @
  loop_tests @
  [
    var_cmp_test;
    if_test;
    funcdef_test;
    no_ret_test;
    not_func_test;
    call_test;
    attribute_test;
    attribute_call_test;
    tuple_test;
    raise_test;
    try_test;
    big_test;
  ] @
  error_tests
