open OUnit2
open Batteries
open Jhupllib
open Python2_parser
open Python2_ast
open Lexing

let annot = Pos.of_pos Lexing.dummy_pos;;

let string_of_stmt e = Pp_utils.pp_to_string
    (Pp_utils.pp_list (pp_stmt (fun _ _ -> ()))) e;;
let equivalent_stmt e1 e2 = List.eq (equal_stmt ( fun _ _ -> true)) e1 e2;;

let string_of_modl m = Pp_utils.pp_to_string
    (pp_modl (fun _ _ -> ())) m;;
let equivalent_modl m1 m2 = equal_modl ( fun _ _ -> true) m1 m2;;

let parse_stmt_from_string_safe str =
  try
    parse_stmt_from_string str
  with
  | Python2_parser.Parse_error p ->
    assert_failure (Printf.sprintf "Error in line %d, col %d."
                      p.pos_lnum p.pos_cnum)
;;

let parse_from_string_safe str =
  try
    parse_from_string str
  with
  | Python2_parser.Parse_error p ->
    assert_failure (Printf.sprintf "Error in line %d, col %d."
                      p.pos_lnum p.pos_cnum)
;;

(* Functions to hide testing boilerplate *)

let gen_module_test (name : string) (prog : string) (expected : 'a stmt list) =
  name>::
  ( fun _ ->
      let actual = parse_from_string_safe (prog ^ "\n") in
      assert_equal ~printer:string_of_modl ~cmp:equivalent_modl
        (Module(expected, annot)) actual
  )

let gen_stmt_test (name : string) (prog : string) (expected : 'a expr) =
  name>::
  ( fun _ ->
      let actual = parse_stmt_from_string_safe (prog ^ "\n") in
      assert_equal ~printer:string_of_stmt ~cmp:equivalent_stmt
        [(Expr(expected, annot))] actual
  )

let int_test = gen_stmt_test "int_test"
    "4"
    (Num(Int(4), annot))
;;

let none_test = gen_stmt_test "none_test"
    "None"
    (NoneExpr(annot))
;;

let unop_test = gen_stmt_test "unop_test_1"
    "+4"
    (UnaryOp(UAdd, Num(Int(4), annot), annot))
;;

let unop_not_test = gen_stmt_test "unop_not_test"
    "not x"
    (UnaryOp(Not, Name("x", Load, annot), annot))
;;

let boolop_and_test = gen_stmt_test "boolop_and_test"
    "x and True and 5"
    (BoolOp(
        And,
        [
          Name("x", Load, annot);
          Bool(true, annot);
          Num(Int(5), annot);
        ],
        annot
      )
    )
;;

let boolop_or_test = gen_stmt_test "boolop_or_test"
    "x or False or 5"
    (BoolOp(
        Or,
        [
          Name("x", Load, annot);
          Bool(false, annot);
          Num(Int(5), annot);
        ],
        annot
      )
    )
;;

let boolop_all_test = gen_stmt_test "boolop_all_test"
    "a and b and not c or d and not c or not a and not b"
    ( (* Expected order of operations is not, then and, then or *)
      BoolOp(
        Or,
        [
          BoolOp(And,
                 [
                   Name("a", Load, annot);
                   Name("b", Load, annot);
                   UnaryOp(Not, Name("c", Load, annot), annot);
                 ],
                 annot);
          BoolOp(And,
                 [
                   Name("d", Load, annot);
                   UnaryOp(Not, Name("c", Load, annot), annot);
                 ],
                 annot);
          BoolOp(And,
                 [
                   UnaryOp(Not, Name("a", Load, annot), annot);
                   UnaryOp(Not, Name("b", Load, annot), annot);
                 ],
                 annot);
        ],
        annot
      )
    )
;;

let var_assign_test = gen_module_test "var_assign_test"
    "x = 5"
    [
      Assign(
        [ Name("x", Store, annot) ],
        Num(Int(5), annot),
        annot
      )
    ]
;;

let var_double_assign_test = gen_module_test "var_double_assign_test"
    "x = y = 5"
    [
      Assign(
        [ Name("x", Store, annot); Name("y", Store, annot) ],
        Num(Int(5), annot),
        annot
      )
    ]
;;

let var_assign_from_tuple_test = gen_module_test "var_assign_from_tuple_test"
    "i, j = (1,2)"
    [
      Assign(
        [
          Tuple(
            [
              Name("i", Store, annot);
              Name("j", Store, annot);
            ],
            Store,
            annot)
        ],
        Tuple(
          [
            Num(Int(1), annot);
            Num(Int(2), annot);
          ],
          Load,
          annot),
        annot)
    ]
;;

let var_aug_assign_test = gen_module_test "var_aug_assign_test"
    "x *= 5"
    [
      AugAssign(
        Name("x", Store, annot),
        Mult,
        Num(Int(5), annot),
        annot
      )
    ]
;;

let var_cmp_test = gen_module_test "var_cmp_test"
    "x <= 42"
    [
      Expr(
        Compare(
          Name("x", Load, annot),
          [LtE],
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
      FunctionDef("test_function",
                  ( (* Args *)
                    [
                      Name("arg1",
                           Param,
                           annot);
                      Name("arg2",
                           Param,
                           annot)
                    ],
                    None,
                    None,
                    []
                    (* End Args *)),

                  [ (* Body *)
                    Return(Some(Name("arg1",
                                     Load,
                                     annot)),
                           annot)
                  ],

                  [
                    (* Decorator list *)
                  ],
                  annot)
    ]
;;

let call_test = gen_stmt_test "call_test"
    "func(1,x,'foo')"
    (Call(
        Name("func", Load, annot),
        [
          Num(Int(1), annot);
          Name("x", Load, annot);
          Str("foo", annot);
        ],
        [],  (* Keywords *)
        None, (* Starargs *)
        None, (* Kwargs *)
        annot))
;;

let attribute_test = gen_stmt_test "attribute_test"
    "obj.member_var"
    (Attribute(
        Name("obj", Load, annot),
        "member_var",
        Load,
        annot))
;;

let attribute_call_test = gen_stmt_test "attribute_test"
    "obj.member_func()"
    (Call(
        Attribute(
          Name("obj", Load, annot),
          "member_func",
          Load,
          annot),
        [],
        [],
        None,
        None,
        annot
      ))
;;

let if_test = gen_module_test "if_test"
    "if x > 2:\n\tx = 3\nelif x < 0: x *= -1\nelse: pass"
    [
      If(
        Compare(Name("x", Load, annot),
                [Gt],
                [Num(Int(2), annot)],
                annot),
        [
          Assign(
            [
              Name("x", Store, annot)
            ],
            Num(Int(3), annot),
            annot
          )
        ],
        [
          If(
            Compare(Name("x", Load, annot),
                    [Lt],
                    [Num(Int(0), annot)],
                    annot),
            [
              AugAssign(
                Name("x", Store, annot),
                Mult,
                Num(Int(-1), annot),
                annot
              )
            ],
            [Pass(annot)],
            annot
          )
        ],
        annot
      )
    ]
;;

let print_test = gen_module_test "print_test"
    "print 1,x,'foo'"
    [
      Print(None,
            [
              Num(Int(1), annot);
              Name("x", Load, annot);
              Str("foo", annot);
            ],
            true,
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
        Load,
        annot
      ))
;;

let while_test = gen_module_test "while_test"
    "while x < 9001:\n\tx = x+1"
    [
      While(
        Compare(
          Name("x", Load, annot),
          [Lt],
          [Num(Int(9001), annot)],
          annot),
        [
          Assign(
            [Name("x", Store, annot)],
            BinOp(Name("x", Load, annot), Add, Num(Int(1), annot), annot),
            annot
          )
        ],
        [],
        annot
      )
    ]
;;

let for_test = gen_module_test "for_test"
    "for i in list:\n\ti+=1"
    [
      For(
        Name("i", Store, annot),
        Name("list", Load, annot),
        [
          AugAssign(
            Name("i", Store, annot),
            Add,
            Num(Int(1), annot),
            annot)
        ],
        [],
        annot);
    ]
;;

let break_test = gen_module_test "break_test"
    "while x < 9001:\n\tbreak"
    [
      While(
        Compare(
          Name("x", Load, annot),
          [Lt],
          [Num(Int(9001), annot)],
          annot),
        [
          Break(annot)
        ],
        [],
        annot
      )
    ]
;;

let continue_test = gen_module_test "continue_test"
    "while x < 9001:\n\tcontinue"
    [
      While(
        Compare(
          Name("x", Load, annot),
          [Lt],
          [Num(Int(9001), annot)],
          annot),
        [
          Continue(annot)
        ],
        [],
        annot
      )
    ]
;;

let raise_test = gen_module_test "raise_test"
    "raise ValueError"
    [Raise(
        Some(Name("ValueError", Load, annot)),
        None,
        None,
        annot)]
;;

let try_block =
  "try:" ^
  "\n\tx = 5" ^
  "\nexcept ValueError:" ^
  "\n\tprint 'Error'" ^
  "\nexcept StopIteration as e:" ^
  "\n\tprint 'Other Error'" ^
  "\n"
;;

let try_test = gen_module_test "try_test"
    try_block
    [(TryExcept (
         [(Assign ([(Name ("x", Store, annot))],
                               (Num ((Int 5), annot)), annot))
         ],
         [(ExceptHandler (
              (Some (Name ("ValueError", Load, annot))),
              None,
              [(Print (None, [(Str ("Error", annot))], true,
                                  annot))
              ],
            annot));
          (ExceptHandler (
              (Some (Name ("StopIteration", Load, annot))),
              (Some (Name ("e", Load, annot))),
              [(Print (None, [(Str ("Other Error", annot))],
                                   true, annot))
              ],
            annot))
         ],
         [], annot))
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
  (FunctionDef(
      "triangle",
      ( (* Args *)
        [Name("n", Param, annot)],
        None,
        None,
        []
      ),
      [ (* Body *)
        Assign(
          [
            Name("count", Store, annot)
          ],
          Num(Int(0), annot),
          annot
        );
        Assign(
          [
            Name("i", Store, annot)
          ],
          Num(Int(0), annot),
          annot
        );
        While(
          Compare(
            Name("count", Load, annot),
            [Lt],
            [Name("n", Load, annot)],
            annot
          ),
          [
            AugAssign(
              Name("i", Store, annot),
              Add,
              Name("count", Load, annot),
              annot
            );
            Assign(
              [Name("count", Store, annot)],
              BinOp(Name("count", Load, annot), Add, Num(Int(1), annot), annot),
              annot
            )
          ],
          [],
          annot
        );
        Return(Some(Name("i", Load, annot)), annot);
      ],
      [], (* Decorator list *)
      annot
    )
  )
;;

let big_test = gen_module_test "big_test"
    (triangle_def ^ "\n[triangle(1),triangle(7)]")
    [
      triangle_ast;
      Expr(List(
          [
            Call(
              Name("triangle", Load, annot),
              [
                Num(Int(1), annot)
              ],
              [],
              None,
              None,
              annot
            );
            Call(
              Name("triangle", Load, annot),
              [
                Num(Int(7), annot)
              ],
              [],
              None,
              None,
              annot
            );
          ],
          Load,
          annot
        ),
           annot)
    ]
;;

(* Tests of lists and slicing *)
let list_str = "[1,2,3,'four','five',2+4]";;
let list_expr = (
  List(
    [
      Num(Int(1),annot);
      Num(Int(2),annot);
      Num(Int(3),annot);
      Str("four", annot);
      Str("five", annot);
      BinOp(Num(Int(2),annot), Add, Num(Int(4),annot), annot);
    ],
    Load,
    annot
  )
)

let list_test = gen_stmt_test "list_test"
    list_str
    list_expr;;

let list_in_test = gen_stmt_test "lst_in_test"
    ("5 in " ^ list_str)
    (Compare(
        Num(Int(5), annot),
        [In],
        [list_expr],
        annot
      )
    )
;;

let gen_slice_test (name : string) (slice : string) (expected_slice: 'a slice) =
  gen_stmt_test
    name
    (list_str ^ slice)
    (Subscript(list_expr, expected_slice, Load, annot))
;;

let list_tests =
  [
    list_test;
    list_in_test;
    (gen_slice_test "slice_test_1" "[0]"
       (Index(Num(Int(0),annot))));
    (gen_slice_test "slice_test2" "[5-2]"
       (Index(BinOp(Num(Int(5),annot), Sub, Num(Int(2), annot), annot))));
    (gen_slice_test "slice_test3" "[2:]"
       (Slice(Some(Num(Int(2), annot)), None, None)));
    (gen_slice_test "slice_test4" "[:4]"
       (Slice(None, Some(Num(Int(4), annot)), None)));
    (gen_slice_test "slice_test5" "[::3]"
       (Slice(None, None, Some(Num(Int(3), annot)))));
    (gen_slice_test "slice_test6" "[2:4]"
       (Slice(Some(Num(Int(2), annot)), Some(Num(Int(4), annot)), None)));
    (gen_slice_test "slice_test7" "[2:4:-1]"
       (Slice(Some(Num(Int(2), annot)),
              Some(Num(Int(4), annot)),
              Some(Num(Int(-1), annot)))));
  ]

(* Tests of various binary operations *)
let gen_binop_test (name : string) (prog : string) (lhs : 'a expr) (rhs : 'a expr) op =
  gen_stmt_test name prog (BinOp(lhs, op, rhs, annot))
;;

let binop_tests =
  [
    (gen_binop_test "add_int_test" "42 + 9001"
       (Num(Int(42), annot)) (Num(Int(9001), annot)) Add);
    (gen_binop_test "add_float_test" "42.0 + 9001.75"
       (Num(Float(42.0), annot)) (Num(Float(9001.75), annot)) Add);
    (gen_binop_test "add_int_float_test" "42 + 9001.5"
       (Num(Int(42), annot)) (Num(Float(9001.5), annot)) Add);

    (gen_binop_test "add_str_test" "'foo' + 'bar'"
       (Str("foo", annot)) (Str("bar", annot)) Add);
    (gen_binop_test "add_int_str_test" "42 + 'foo'"
       (Num(Int(42), annot)) (Str("foo", annot)) Add);

    (gen_binop_test "sub_int_test" "42 - 9001"
       (Num(Int(42), annot)) (Num(Int(9001), annot)) Sub);
    (gen_binop_test "mult_int_test" "42 * 9001"
       (Num(Int(42), annot)) (Num(Int(9001), annot)) Mult);
    (gen_binop_test "div_int_test" "42 / 9001"
       (Num(Int(42), annot)) (Num(Int(9001), annot)) Div);
    (gen_binop_test "mod_int_test" "42 % 9001"
       (Num(Int(42), annot)) (Num(Int(9001), annot)) Mod);
    (gen_binop_test "pow_int_test" "42 ** 9001"
       (Num(Int(42), annot)) (Num(Int(9001), annot)) Pow);

    (gen_binop_test "triple_binop_test" "(42 - 9001) + 17"
       (BinOp(Num(Int(42), annot),
              Sub,
              Num(Int(9001), annot),
              annot))
       (Num(Int(17), annot))
       Add);

    (gen_binop_test "order_of_operations_test" "1+2*3"
       (Num(Int(1), annot))
       (BinOp(Num(Int(2), annot),
              Mult,
              Num(Int(3), annot),
              annot))
       Add);
  ]

(* Run the tests *)

let tests =
  "parser">:::
  [
    int_test;
    none_test;
    unop_test;
    unop_not_test;
    boolop_and_test;
    boolop_or_test;
    boolop_all_test;
    var_assign_test;
    var_double_assign_test;
    var_assign_from_tuple_test;
    var_aug_assign_test;
    var_cmp_test;
    if_test;
    funcdef_test;
    call_test;
    attribute_test;
    attribute_call_test;
    tuple_test;
    print_test;
    while_test;
    for_test;
    break_test;
    continue_test;
    raise_test;
    try_test;
    big_test;
  ]
  @ binop_tests
  @ list_tests
