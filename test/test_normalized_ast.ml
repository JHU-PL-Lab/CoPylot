open OUnit2
open Batteries
open Jhupllib
open Python2_parser
open Lexing
module Simplified = Python2_simplified_ast
open Python2_normalized_ast
module Lift = Python2_analysis_conversion
module Simplify = Python2_ast_simplifier
module Normalize = Python2_ast_normalizer

let dummy_uid = 0;;

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

let gen_module_test (name : string) (prog : string) (expected : stmt list) =
  name>::
  ( fun _ ->
      let concrete = parse_from_string_safe (prog ^ "\n") in
      let abstract = Lift.lift_modl concrete in
      let simplified = Simplify.simplify_modl abstract in
      Simplify.reset_unique_name ();
      let actual = Normalize.normalize_modl simplified in
      Normalize.reset_unique_name ();
      assert_equal ~printer:string_of_modl ~cmp:equivalent_modl
        (Module(expected, 0)) actual (* FIXME: Add uids *)
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
     Simplify.reset_unique_name ();
     assert_raises
       expected
       (fun _ ->
          Normalize.normalize_modl simplified)
  )
;;

let int_test = gen_module_test "int_test"
    "4"
    [SimpleExprStmt(Num(Int(Pos), dummy_uid), dummy_uid)]
;;

let float_test = gen_module_test "float_test"
    "1.7"
    [SimpleExprStmt(Num(Float(Pos), dummy_uid), dummy_uid)]
;;

let float_zero_test = gen_module_test "float_zero_test"
    "0.0"
    [SimpleExprStmt(Num(Float(Zero), dummy_uid), dummy_uid)]
;;

let unop_test = gen_module_test "unop_test"
    "+4"
    [
      Assign("$normalized_unique_name_0",
             UnaryOp(UAdd, Num(Int(Pos), dummy_uid), dummy_uid),
             dummy_uid);
      SimpleExprStmt(Name("$normalized_unique_name_0", dummy_uid), dummy_uid);
    ]

;;

let unop_not_test = gen_module_test "unop_not_test"
    "not x"
    [
      Assign("$normalized_unique_name_0",
             UnaryOp(Not, Name("x", dummy_uid), dummy_uid),
             dummy_uid);
      SimpleExprStmt(Name("$normalized_unique_name_0", dummy_uid), dummy_uid);
    ]
;;

let boolop_and_test = gen_module_test "boolop_and_test"
    "1+2 and x and -5"
    [
      Assign("$normalized_unique_name_0",
             BinOp(
               Num(Int(Pos), dummy_uid),
               Add,
               Num(Int(Pos), dummy_uid),
               dummy_uid),
             dummy_uid);

      If(Name("$normalized_unique_name_0", dummy_uid),
         [
           Assign("$normalized_unique_name_1",
                  SimpleExpr(Name("x", dummy_uid), dummy_uid),
                  dummy_uid);
         ],
         [
           Assign("$normalized_unique_name_1",
                  SimpleExpr(Bool(false, dummy_uid), dummy_uid),
                  dummy_uid);
         ],
         dummy_uid);

      Assign("$normalized_unique_name_2",
             BoolOp(
               Name("$normalized_unique_name_0", dummy_uid),
               And,
               Name("$normalized_unique_name_1", dummy_uid),
               dummy_uid),
             dummy_uid);

      If(Name("$normalized_unique_name_2", dummy_uid),
         [
           Assign("$normalized_unique_name_3",
                  SimpleExpr(Num(Int(Neg), dummy_uid), dummy_uid),
                  dummy_uid);
         ],
         [
           Assign("$normalized_unique_name_3",
                  SimpleExpr(Bool(false, dummy_uid), dummy_uid),
                  dummy_uid);
         ],
         dummy_uid);

      Assign("$normalized_unique_name_4",
             BoolOp(
               Name("$normalized_unique_name_2", dummy_uid),
               And,
               Name("$normalized_unique_name_3", dummy_uid),
               dummy_uid),
             dummy_uid);

      SimpleExprStmt(Name("$normalized_unique_name_4", dummy_uid), dummy_uid);
    ]
;;

let boolop_or_test = gen_module_test "boolop_or_test"
    "x or False or 0"
    [
      If(Name("x", dummy_uid),
         [
           Assign("$normalized_unique_name_0",
                  SimpleExpr(Bool(true, dummy_uid), dummy_uid),
                  dummy_uid);
         ],
         [
           Assign("$normalized_unique_name_0",
                  SimpleExpr(Bool(false, dummy_uid), dummy_uid),
                  dummy_uid);
         ],
         dummy_uid);

      Assign("$normalized_unique_name_1",
             BoolOp(
               Name("x", dummy_uid),
               Or,
               Name("$normalized_unique_name_0", dummy_uid),
               dummy_uid),
             dummy_uid);

      If(Name("$normalized_unique_name_1", dummy_uid),
         [
           Assign("$normalized_unique_name_2",
                  SimpleExpr(Bool(true, dummy_uid), dummy_uid),
                  dummy_uid);
         ],
         [
           Assign("$normalized_unique_name_2",
                  SimpleExpr(Num(Int(Zero), dummy_uid), dummy_uid),
                  dummy_uid);
         ],
         dummy_uid);

      Assign("$normalized_unique_name_3",
             BoolOp(
               Name("$normalized_unique_name_1", dummy_uid),
               Or,
               Name("$normalized_unique_name_2", dummy_uid),
               dummy_uid),
             dummy_uid);

      SimpleExprStmt(Name("$normalized_unique_name_3", dummy_uid), dummy_uid);
    ]
;;

(* TODO: Translate this if you have a spare hour or so
   let boolop_all_test = gen_stmt_test "boolop_all_test"
    "a and b and not c or d and not c or not a and not b"
    ( (* Expected order of operations is not, then and, then or *)
      BoolOp(
        Or,
        [
          BoolOp(And,
                 [
                   Name("a", dummy_uid);
                   Name("b", dummy_uid);
                   UnaryOp(Not, Name("c", dummy_uid), dummy_uid);
                 ],
                 dummy_uid);
          BoolOp(And,
                 [
                   Name("d", dummy_uid);
                   UnaryOp(Not, Name("c", dummy_uid), dummy_uid);
                 ],
                 dummy_uid);
          BoolOp(And,
                 [
                   UnaryOp(Not, Name("a", dummy_uid), dummy_uid);
                   UnaryOp(Not, Name("b", dummy_uid), dummy_uid);
                 ],
                 dummy_uid);
        ],
        dummy_uid
      )
    )
   ;;
*)

let var_assign_test = gen_module_test "var_assign_test"
    "x = 5"
    [
      Assign(
        "$simplified_unique_name_0",
        SimpleExpr(Num(Int(Pos), dummy_uid), dummy_uid),
        dummy_uid
      );
      Assign(
        "x",
        SimpleExpr(Name("$simplified_unique_name_0", dummy_uid), dummy_uid),
        dummy_uid
      )
    ]
;;

let var_double_assign_test = gen_module_test "var_double_assign_test"
    "x = y = 5"
    [
      Assign(
        "$simplified_unique_name_0",
        SimpleExpr(Num(Int(Pos), dummy_uid), dummy_uid),
        dummy_uid
      );
      Assign(
        "x",
        SimpleExpr(Name("$simplified_unique_name_0", dummy_uid), dummy_uid),
        dummy_uid
      );
      Assign(
        "y",
        SimpleExpr(Name("$simplified_unique_name_0", dummy_uid), dummy_uid),
        dummy_uid
      )
    ]
;;

(*
let assign_iterator obj num =
  Assign(
    "$unique_name_" ^ string_of_int num,
    Attribute(
      Call(
        Attribute(
          obj,
          "__iter__",
          dummy_uid
        ),
        [],
        dummy_uid
      ),
      "next",
      dummy_uid),
    dummy_uid
  )
;;
TODO: This
let var_assign_from_tuple_test = gen_module_test "var_assign_from_tuple_test"
    "i, j = (-1,0)"
    [
      Assign(
        "$unique_name_0",
        Tuple(
          [
            Num(Int(Neg), dummy_uid);
            Num(Int(Zero), dummy_uid);
          ],
          dummy_uid),
        dummy_uid);

      assign_iterator (Name("$unique_name_0", dummy_uid)) 1;

      TryExcept(
        [
          Assign(
            "$unique_name_2",
            Call(Name("$unique_name_1", dummy_uid), [], dummy_uid),
            dummy_uid);
          Assign(
            "$unique_name_3",
            Call(Name("$unique_name_1", dummy_uid), [], dummy_uid),
            dummy_uid);
          TryExcept(
            [
              Expr(
                Call(Name("$unique_name_1", dummy_uid), [], dummy_uid),
                dummy_uid
              );
              Raise(
                Some(Name("ValueError", dummy_uid)),
                Some(Str(StringLiteral("too many values to unpack"),
                         dummy_uid)),
                dummy_uid);
            ],
            [ExceptHandler(
                Some(Name("StopIteration", dummy_uid)),
                None,
                [
                  Pass(dummy_uid)
                ],
                dummy_uid
              )
            ],
            dummy_uid
          )
        ],
        [ExceptHandler(
            Some(Name("StopIteration", dummy_uid)),
            None,
            [
              Raise(
                Some(Name("ValueError", dummy_uid)),
                Some(Str(StringAbstract, dummy_uid)),
                dummy_uid)
            ],
            dummy_uid
          )],
        dummy_uid);
      Assign(
        "$unique_name_4",
        Name("$unique_name_2", dummy_uid),
        dummy_uid);
      Assign(
        "i",
        Name("$unique_name_4", dummy_uid),
        dummy_uid);
      Assign(
        "$unique_name_5",
        Name("$unique_name_3", dummy_uid),
        dummy_uid);
      Assign(
        "j",
        Name("$unique_name_5", dummy_uid),
        dummy_uid);
    ]
;;
*)

let assign_to_index_test = gen_module_test "assign_to_index_test"
    "list[1+2] = 3"
    [
      Assign(
        "$simplified_unique_name_0",
        SimpleExpr(Num(Int(Pos), dummy_uid), dummy_uid),
        dummy_uid);

      Assign(
        "$normalized_unique_name_0",
        Attribute(
          Name("list", dummy_uid),
          "__setitem__",
          dummy_uid),
        dummy_uid);

      Assign(
        "$normalized_unique_name_1",
        BinOp(
          Num(Int(Pos), dummy_uid),
          Add,
          Num(Int(Pos), dummy_uid),
          dummy_uid),
        dummy_uid);

      Assign(
        "$normalized_unique_name_2",
        Call(
          Name("$normalized_unique_name_0", dummy_uid),
          [
            Name("$normalized_unique_name_1", dummy_uid);
            Name("$simplified_unique_name_0", dummy_uid);
          ],
          dummy_uid
        ),
        dummy_uid
      );

      SimpleExprStmt(
        Name("$normalized_unique_name_2", dummy_uid),
        dummy_uid);
    ]
;;

let assign_to_slice_test = gen_module_test "assign_to_slice_test"
    "list[1:2] = 3"
    [
      Assign(
        "$simplified_unique_name_0",
        SimpleExpr(Num(Int(Pos), dummy_uid), dummy_uid),
        dummy_uid);

      Assign(
        "$normalized_unique_name_0",
        Attribute(
          Name("list", dummy_uid),
          "__setitem__",
          dummy_uid),
        dummy_uid);

      Assign(
        "$normalized_unique_name_1",
        Call(
          Name("slice", dummy_uid),
          [
            Num(Int(Pos), dummy_uid);
            Num(Int(Pos), dummy_uid);
            Name("None", dummy_uid);
          ],
          dummy_uid
        ),
        dummy_uid);

      Assign(
        "$normalized_unique_name_2",
        Call(
          Name("$normalized_unique_name_0", dummy_uid),
          [
            Name("$normalized_unique_name_1", dummy_uid);
            Name("$simplified_unique_name_0", dummy_uid);
          ],
          dummy_uid
        ),
        dummy_uid
      );

      SimpleExprStmt(
        Name("$normalized_unique_name_2", dummy_uid),
        dummy_uid);
    ]
;;

let assign_to_attribute_test = gen_module_test "assign_to_attribute_test"
    "obj.member = 7"
    [
      Assign(
        "$simplified_unique_name_0",
        SimpleExpr(Num(Int(Pos), dummy_uid), dummy_uid),
        dummy_uid);

      Assign(
        "$normalized_unique_name_0",
        Attribute(
          Name("obj", dummy_uid),
          "__setattr__",
          dummy_uid
        ),
        dummy_uid);

      Assign(
        "$normalized_unique_name_1",
        Call(
          Name("$normalized_unique_name_0", dummy_uid),
          [
            Str(StringLiteral("member"), dummy_uid);
            Name("$simplified_unique_name_0", dummy_uid);
          ],
          dummy_uid
        ),
        dummy_uid
      );

      SimpleExprStmt(Name("$normalized_unique_name_1", dummy_uid),
                     dummy_uid);

    ]
;;

let var_aug_assign_test = gen_module_test "var_aug_assign_test"
    "x *= -5"
    [
      Assign(
        "$normalized_unique_name_0",
        BinOp(Name("x", dummy_uid),
              Mult,
              Num(Int(Neg), dummy_uid),
              dummy_uid
             ),
        dummy_uid
      );

      Assign(
        "$simplified_unique_name_0",
        SimpleExpr(Name("$normalized_unique_name_0", dummy_uid), dummy_uid),
        dummy_uid
      );

      Assign(
        "x",
        SimpleExpr(Name("$simplified_unique_name_0", dummy_uid), dummy_uid),
        dummy_uid
      )
    ]
;;

let var_cmp_test = gen_module_test "var_cmp_test"
    "x <= 9000+1 > True"
    [
      If(Bool(true, dummy_uid),
         [
           Assign(
             "$normalized_unique_name_0",
             BinOp(
               Num(Int(Pos), dummy_uid),
               Add,
               Num(Int(Pos), dummy_uid),
               dummy_uid),
             dummy_uid);

           Assign(
             "$normalized_unique_name_1",
             Compare(
               Name("x", dummy_uid),
               LtE,
               Name("$normalized_unique_name_0", dummy_uid),
               dummy_uid
             ),
             dummy_uid)
         ],
         [
           Assign(
             "$normalized_unique_name_1",
             SimpleExpr(Bool(false, dummy_uid), dummy_uid),
             dummy_uid)
         ],
         dummy_uid);

      Assign(
        "$normalized_unique_name_2",
        BoolOp(
          Bool(true, dummy_uid),
          And,
          Name("$normalized_unique_name_1", dummy_uid),
          dummy_uid),
        dummy_uid
      );

      If(
        Name("$normalized_unique_name_2", dummy_uid),
        [
          Assign(
            "$normalized_unique_name_3",
            Compare(
              Name("$normalized_unique_name_0", dummy_uid),
              Gt,
              Bool(true, dummy_uid),
              dummy_uid
            ),
            dummy_uid)
        ],
        [
          Assign(
            "$normalized_unique_name_3",
            SimpleExpr(Bool(false, dummy_uid), dummy_uid),
            dummy_uid)
        ],
        dummy_uid);

      Assign(
        "$normalized_unique_name_4",
        BoolOp(
          Name("$normalized_unique_name_2", dummy_uid),
          And,
          Name("$normalized_unique_name_3", dummy_uid),
          dummy_uid),
        dummy_uid
      );

      SimpleExprStmt(
        Name("$normalized_unique_name_4", dummy_uid),
        dummy_uid);
    ]
;;

let funcdef_test = gen_module_test "funcdef_test"
    "def test_function(arg1,arg2):\n\treturn arg1"
    [
      FunctionDef("test_function",
                  [
                    "arg1";
                    "arg2";
                  ],
                  [ (* Body *)
                    Return(Some(Name("arg1", dummy_uid)),
                           dummy_uid)
                  ],
                  dummy_uid)
    ]
;;

let call_test = gen_module_test "call_test"
    "func(1,x-1,get_arg('foo'))"
    [
      Assign(
        "$normalized_unique_name_0",
        BinOp(
          Name("x", dummy_uid),
          Sub,
          Num(Int(Pos), dummy_uid),
          dummy_uid
        ),
        dummy_uid
      );

      Assign(
        "$normalized_unique_name_1",
        Call(
          Name("get_arg", dummy_uid),
          [Str(StringLiteral("foo"), dummy_uid)],
          dummy_uid
        ),
        dummy_uid
      );

      Assign(
        "$normalized_unique_name_2",
        Call(
          Name("func", dummy_uid),
          [
            Num(Int(Pos), dummy_uid);
            Name("$normalized_unique_name_0", dummy_uid);
            Name("$normalized_unique_name_1", dummy_uid);
          ],
          dummy_uid),
        dummy_uid
      );

      SimpleExprStmt(
        Name("$normalized_unique_name_2", dummy_uid),
        dummy_uid);
    ]
;;

let attribute_test = gen_module_test "attribute_test"
    "obj.member_var"
    [
      Assign(
        "$normalized_unique_name_0",
        Attribute(
          Name("obj", dummy_uid),
          "member_var",
          dummy_uid),
        dummy_uid);

      SimpleExprStmt(
        Name("$normalized_unique_name_0", dummy_uid),
        dummy_uid);
    ]
;;

let attribute_call_test = gen_module_test "attribute_call_test"
    "obj.member_func()"
    [
      Assign(
        "$normalized_unique_name_0",
        Attribute(
          Name("obj", dummy_uid),
          "member_func",
          dummy_uid),
        dummy_uid);

      Assign(
        "$normalized_unique_name_1",
        Call(
          Name("$normalized_unique_name_0", dummy_uid),
          [],
          dummy_uid),
        dummy_uid);

      SimpleExprStmt(
        Name("$normalized_unique_name_1", dummy_uid),
        dummy_uid);
    ]
;;

let if_test = gen_module_test "if_test"
    "if x > 1+1:\n\tx = foo()\nelif x < 0.0: error('x < 0')\nelse: pass"
    [
      If(
        Bool(true, dummy_uid),
        [
          Assign(
            "$normalized_unique_name_0",
            BinOp(
              Num(Int(Pos), dummy_uid),
              Add,
              Num(Int(Pos), dummy_uid),
              dummy_uid),
            dummy_uid);

          Assign(
            "$normalized_unique_name_1",
            Compare(
              Name("x", dummy_uid),
              Gt,
              Name("$normalized_unique_name_0", dummy_uid),
              dummy_uid),
            dummy_uid);
        ],
        [
          Assign(
            "$normalized_unique_name_1",
            SimpleExpr(Bool(false, dummy_uid), dummy_uid),
            dummy_uid);
        ],
        dummy_uid);

      Assign(
        "$normalized_unique_name_2",
        BoolOp(
          Bool(true, dummy_uid),
          And,
          Name("$normalized_unique_name_1", dummy_uid),
          dummy_uid),
        dummy_uid);

      If(
        Name("$normalized_unique_name_2", dummy_uid),
        [
          Assign(
            "$normalized_unique_name_3",
            Call(
              Name("foo", dummy_uid),
              [],
              dummy_uid),
            dummy_uid);

          Assign(
            "$simplified_unique_name_0",
            SimpleExpr(
              Name("$normalized_unique_name_3", dummy_uid),
              dummy_uid),
            dummy_uid);

          Assign(
            "x",
            SimpleExpr(
              Name("$simplified_unique_name_0", dummy_uid),
              dummy_uid),
            dummy_uid);
        ],
        [
          If( (* Computing x < 0.0 *)
            Bool(true, dummy_uid),
            [
              Assign(
                "$normalized_unique_name_4",
                Compare(
                  Name("x", dummy_uid),
                  Lt,
                  Num(Float(Zero), dummy_uid),
                  dummy_uid),
                dummy_uid);
            ],
            [
              Assign(
                "$normalized_unique_name_4",
                SimpleExpr(Bool(false, dummy_uid), dummy_uid),
                dummy_uid);
            ],
            dummy_uid);

          Assign(
            "$normalized_unique_name_5",
            BoolOp(
              Bool(true, dummy_uid),
              And,
              Name("$normalized_unique_name_4", dummy_uid),
              dummy_uid),
            dummy_uid);

          If( (* Elif stmt *)
            Name("$normalized_unique_name_5", dummy_uid),
            [
              Assign(
                "$normalized_unique_name_6",
                Call(
                  Name("error", dummy_uid),
                  [Str(StringLiteral("x < 0"), dummy_uid)],
                  dummy_uid),
                dummy_uid);

              SimpleExprStmt(
                Name("$normalized_unique_name_6", dummy_uid),
                dummy_uid);
            ],
            [
              Pass(dummy_uid);
            ],
            dummy_uid)
        ],
        dummy_uid
      )
    ]
;;

let print_test = gen_module_test "print_test"
    "print 1,x(1+1),'foo'"
    [
      Assign(
        "$normalized_unique_name_0",
        BinOp(
          Num(Int(Pos), dummy_uid),
          Add,
          Num(Int(Pos), dummy_uid),
          dummy_uid),
        dummy_uid);

      Assign(
        "$normalized_unique_name_1",
        Call(
          Name("x", dummy_uid),
          [Name("$normalized_unique_name_0", dummy_uid)],
          dummy_uid),
        dummy_uid);

      Print(None,
            [
              Num(Int(Pos), dummy_uid);
              Name("$normalized_unique_name_1", dummy_uid);
              Str(StringLiteral("foo"),dummy_uid);
            ],
            true,
            dummy_uid)
    ]
;;

let tuple_test = gen_module_test "tuple_test"
    "(1,1+1,(2,f(2)),'foo')"
    [
      Assign( (* 1+1 *)
        "$normalized_unique_name_0",
        BinOp(
          Num(Int(Pos), dummy_uid),
          Add,
          Num(Int(Pos), dummy_uid),
          dummy_uid),
        dummy_uid);

      Assign( (* f(2) *)
        "$normalized_unique_name_1",
        Call(
          Name("f", dummy_uid),
          [Num(Int(Pos), dummy_uid)],
          dummy_uid),
        dummy_uid);

      Assign( (* (2, f(2)) *)
        "$normalized_unique_name_2",
        Tuple(
          [
            Num(Int(Pos), dummy_uid);
            Name("$normalized_unique_name_1", dummy_uid);
          ],
          dummy_uid),
        dummy_uid);

      Assign(
        "$normalized_unique_name_3",
        Tuple(
          [
            Num(Int(Pos), dummy_uid);
            Name("$normalized_unique_name_0", dummy_uid);
            Name("$normalized_unique_name_2", dummy_uid);
            Str(StringLiteral("foo"), dummy_uid);
          ],
          dummy_uid),
        dummy_uid);

      SimpleExprStmt(
        Name("$normalized_unique_name_3", dummy_uid),
        dummy_uid);
    ]
;;

let while_test = gen_module_test "while_test"
    "while x < 9001:\n\tx = x+1"
    [
      Pass(dummy_uid);

      If(
        Bool(true, dummy_uid),
        [
          Assign(
            "$normalized_unique_name_0",
            Compare(
              Name("x", dummy_uid),
              Lt,
              Num(Int(Pos), dummy_uid),
              dummy_uid),
            dummy_uid);
        ],
        [
          Assign(
            "$normalized_unique_name_0",
            SimpleExpr(Bool(false, dummy_uid), dummy_uid),
            dummy_uid);
        ],
        dummy_uid);

      Assign(
        "$normalized_unique_name_1",
        BoolOp(
          Bool(true, dummy_uid),
          And,
          Name("$normalized_unique_name_0", dummy_uid),
          dummy_uid),
        dummy_uid);

      Assign(
        "$normalized_unique_name_2",
        UnaryOp(
          Not,
          Name("$normalized_unique_name_1", dummy_uid),
          dummy_uid),
        dummy_uid);

      If(
        Name("$normalized_unique_name_2", dummy_uid),
        [
          Goto(dummy_uid, dummy_uid);
        ],
        [],
        dummy_uid
      );

      Assign(
        "$normalized_unique_name_3",
        BinOp(Name("x", dummy_uid),
              Add,
              Num(Int(Pos), dummy_uid),
              dummy_uid
             ),
        dummy_uid);

      Assign(
        "$simplified_unique_name_0",
        SimpleExpr(
          Name("$normalized_unique_name_3", dummy_uid),
          dummy_uid),
        dummy_uid);

      Assign(
        "x",
        SimpleExpr(
          Name("$simplified_unique_name_0", dummy_uid),
          dummy_uid),
        dummy_uid);

      Goto(dummy_uid, dummy_uid);

      Pass(dummy_uid);
    ]
;;

let for_test = gen_module_test "for_test"
    "for i in list:\n\tf(i)"
    [
      Assign(
        "$normalized_unique_name_0",
        Attribute(
          Name("list", dummy_uid),
          "__iter__",
          dummy_uid),
        dummy_uid);

      Assign(
        "$normalized_unique_name_1",
        Call(
          Name("$normalized_unique_name_0", dummy_uid),
          [],
          dummy_uid),
        dummy_uid);

      Assign(
        "$normalized_unique_name_2",
        Attribute(
          Name("$normalized_unique_name_1", dummy_uid),
          "next",
          dummy_uid),
        dummy_uid);

      Assign(
        "$simplified_unique_name_1",
        SimpleExpr(
          Name("$normalized_unique_name_2", dummy_uid),
          dummy_uid),
        dummy_uid);

      Assign(
        "$simplified_unique_name_0",
        SimpleExpr(
          Name("$simplified_unique_name_1", dummy_uid),
          dummy_uid),
        dummy_uid);

      (* TODO
         TryExcept(
         [
          While(
            Bool(true, dummy_uid),
            [
              Assign(
                "$simplified_unique_name_2",
                Call(Name("$simplified_unique_name_0", dummy_uid), [], dummy_uid),
                dummy_uid
              );
              Assign(
                "i",
                Name("$simplified_unique_name_2", dummy_uid),
                dummy_uid
              );
              Expr(Call(Name("f", dummy_uid), [Name("i", dummy_uid)], dummy_uid), dummy_uid);
            ],
            dummy_uid
          )
         ],
         [
          ExceptHandler(
            Some(Name("StopIteration", dummy_uid)),
            None,
            [Pass(dummy_uid)],
            dummy_uid
          )
         ],
         dummy_uid
         )*)
    ]
;;

let break_test = gen_module_test "break_test"
    "while x < 9001:\n\tbreak"
    [
      Pass(dummy_uid);

      If(
        Bool(true, dummy_uid),
        [
          Assign(
            "$normalized_unique_name_0",
            Compare(
              Name("x", dummy_uid),
              Lt,
              Num(Int(Pos), dummy_uid),
              dummy_uid),
            dummy_uid);
        ],
        [
          Assign(
            "$normalized_unique_name_0",
            SimpleExpr(Bool(false, dummy_uid), dummy_uid),
            dummy_uid);
        ],
        dummy_uid);

      Assign(
        "$normalized_unique_name_1",
        BoolOp(
          Bool(true, dummy_uid),
          And,
          Name("$normalized_unique_name_0", dummy_uid),
          dummy_uid),
        dummy_uid);

      Assign(
        "$normalized_unique_name_2",
        UnaryOp(
          Not,
          Name("$normalized_unique_name_1", dummy_uid),
          dummy_uid),
        dummy_uid);

      If(
        Name("$normalized_unique_name_2", dummy_uid),
        [
          Goto(dummy_uid, dummy_uid);
        ],
        [],
        dummy_uid
      );

      Goto(dummy_uid, dummy_uid);

      Goto(dummy_uid, dummy_uid);

      Pass(dummy_uid);
    ]
;;

let continue_test = gen_module_test "continue_test"
    "while x < 9001:\n\tcontinue"
    [
      Pass(dummy_uid);

      If(
        Bool(true, dummy_uid),
        [
          Assign(
            "$normalized_unique_name_0",
            Compare(
              Name("x", dummy_uid),
              Lt,
              Num(Int(Pos), dummy_uid),
              dummy_uid),
            dummy_uid);
        ],
        [
          Assign(
            "$normalized_unique_name_0",
            SimpleExpr(Bool(false, dummy_uid), dummy_uid),
            dummy_uid);
        ],
        dummy_uid);

      Assign(
        "$normalized_unique_name_1",
        BoolOp(
          Bool(true, dummy_uid),
          And,
          Name("$normalized_unique_name_0", dummy_uid),
          dummy_uid),
        dummy_uid);

      Assign(
        "$normalized_unique_name_2",
        UnaryOp(
          Not,
          Name("$normalized_unique_name_1", dummy_uid),
          dummy_uid),
        dummy_uid);

      If(
        Name("$normalized_unique_name_2", dummy_uid),
        [
          Goto(dummy_uid, dummy_uid);
        ],
        [],
        dummy_uid
      );

      Goto(dummy_uid, dummy_uid);

      Goto(dummy_uid, dummy_uid);

      Pass(dummy_uid);
    ]
;;

let bad_break_test = expect_error_test "bad_break_test"
    "break"
    (Failure("'break' outside loop"))
;;

let bad_continue_test = expect_error_test "bad_continue_test"
    "continue"
    (Failure("'continue' not properly in loop"))
;;

(*
let raise_test_no_args = gen_module_test "raise_test_no_args"
    "raise"
    [Raise(None, None, dummy_uid)]
;;

let raise_test_one_arg = gen_module_test "raise_test_no_args"
    "raise ValueError"
    [Raise(
        Some(Name("ValueError", dummy_uid)),
        None,
        dummy_uid)]
;;

let raise_test_two_args = gen_module_test "raise_test_no_args"
    "raise ValueError, 5"
    [Raise(
        Some(Name("ValueError", dummy_uid)),
        Some(Num(Int(Pos), dummy_uid)),
        dummy_uid)]
;;
*)
(*
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
    [
      TryExcept(
        [
          Assign(
            "$unique_name_0",
            Num(Int(Pos), dummy_uid),
            dummy_uid);
          Assign(
            "x",
            Name("$unique_name_0", dummy_uid),
            dummy_uid)
        ],
        [
          ExceptHandler(
            Some(Name("ValueError", dummy_uid)),
            None,
            [
              Print(None,
                    [Str (StringLiteral("Error"), dummy_uid)],
                    true,
                    dummy_uid)
            ],
            dummy_uid);
          ExceptHandler(
            Some(Name("StopIteration", dummy_uid)),
            Some("e"),
            [
              Print (None,
                     [Str(StringLiteral("Other Error"),dummy_uid)],
                     true,
                     dummy_uid)
            ],
            dummy_uid)
        ],
        dummy_uid)
    ]
;;
*)

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
  FunctionDef(
    "triangle",
    ["n"],
    [ (* Body *)
      Assign(
        "$simplified_unique_name_0",
        SimpleExpr(Num(Int(Zero), dummy_uid), dummy_uid),
        dummy_uid);

      Assign(
        "count",
        SimpleExpr(Name("$simplified_unique_name_0", dummy_uid),
                   dummy_uid),
        dummy_uid);

      Assign(
        "$simplified_unique_name_1",
        SimpleExpr(Num(Int(Zero), dummy_uid), dummy_uid),
        dummy_uid);

      Assign(
        "i",
        SimpleExpr(Name("$simplified_unique_name_1", dummy_uid),
                   dummy_uid),
        dummy_uid
      );

      Pass(dummy_uid);

      If(
        Bool(true, dummy_uid),
        [
          Assign(
            "$normalized_unique_name_0",
            Compare(
              Name("count", dummy_uid),
              Lt,
              Name("n", dummy_uid),
              dummy_uid),
            dummy_uid);
        ],
        [
          Assign(
            "$normalized_unique_name_0",
            SimpleExpr(Bool(false, dummy_uid), dummy_uid),
            dummy_uid);
        ],
        dummy_uid);

      Assign(
        "$normalized_unique_name_1",
        BoolOp(
          Bool(true, dummy_uid),
          And,
          Name("$normalized_unique_name_0", dummy_uid),
          dummy_uid),
        dummy_uid);

      Assign(
        "$normalized_unique_name_2",
        UnaryOp(
          Not,
          Name("$normalized_unique_name_1", dummy_uid),
          dummy_uid),
        dummy_uid);

      If(
        Name("$normalized_unique_name_2", dummy_uid),
        [
          Goto(dummy_uid, dummy_uid);
        ],
        [],
        dummy_uid
      );

      Assign(
        "$normalized_unique_name_3",
        BinOp(Name("i", dummy_uid),
              Add,
              Name("count", dummy_uid),
              dummy_uid),
        dummy_uid);

      Assign(
        "$simplified_unique_name_2",
        SimpleExpr(Name("$normalized_unique_name_3", dummy_uid),
                   dummy_uid),
        dummy_uid);

      Assign(
        "i",
        SimpleExpr(
          Name("$simplified_unique_name_2", dummy_uid),
          dummy_uid),
        dummy_uid);

      Assign(
        "$normalized_unique_name_4",
        BinOp(Name("count", dummy_uid),
              Add,
              Num(Int(Pos), dummy_uid),
              dummy_uid),
        dummy_uid);

      Assign(
        "$simplified_unique_name_3",
        SimpleExpr(Name("$normalized_unique_name_4", dummy_uid),
                   dummy_uid),
        dummy_uid);

      Assign(
        "count",
        SimpleExpr(
          Name("$simplified_unique_name_3", dummy_uid),
          dummy_uid),
        dummy_uid);

      Goto(dummy_uid, dummy_uid);

      Pass(dummy_uid);

      Return(Some(Name("i", dummy_uid)), dummy_uid);
    ],
    dummy_uid
  )
;;

let big_test = gen_module_test "big_test"
    (triangle_def ^ "\n[triangle(1),triangle(7)]")
    [
      triangle_ast;

      Assign(
        "$normalized_unique_name_5",
        Call(
          Name("triangle", dummy_uid),
          [Num(Int(Pos), dummy_uid)],
          dummy_uid),
        dummy_uid);

      Assign(
        "$normalized_unique_name_6",
        Call(
          Name("triangle", dummy_uid),
          [Num(Int(Pos), dummy_uid)],
          dummy_uid),
        dummy_uid);

      Assign(
        "$normalized_unique_name_7",
        List(
          [
            Name("$normalized_unique_name_5", dummy_uid);
            Name("$normalized_unique_name_6", dummy_uid);
          ],
          dummy_uid),
        dummy_uid);

      SimpleExprStmt(
        Name("$normalized_unique_name_7", dummy_uid),
        dummy_uid);
    ]
;;
(*
(* Tests of lists and slicing *)
let list_str = "[1,2,3,'four','five',2+4]";;
let list_expr =
  List(
    [
      Num(Int(Pos),dummy_uid);
      Num(Int(Pos),dummy_uid);
      Num(Int(Pos),dummy_uid);
      Str(StringLiteral("four"), dummy_uid);
      Str(StringLiteral("five"), dummy_uid);
      BinOp(Num(Int(Pos),dummy_uid), Add, Num(Int(Pos),dummy_uid), dummy_uid);
    ],
    dummy_uid
  )

let list_test = gen_stmt_test "list_test"
    list_str
    list_expr;;

let list_in_test = gen_stmt_test "lst_in_test"
    ("5 in " ^ list_str)
    (Compare(
        Num(Int(Pos), dummy_uid),
        [In],
        [list_expr],
        dummy_uid
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
                  dummy_uid),
        [ expected_slice ],
        dummy_uid))
;;

let list_tests =
  [
    list_test;
    list_in_test;
    (gen_slice_test "slice_test_1" "[0]"
       (Num(Int(Zero),dummy_uid)));
    (gen_slice_test "slice_test2" "[5-2]"
       (BinOp(Num(Int(Pos),dummy_uid), Sub, Num(Int(Pos), dummy_uid), dummy_uid)));
    (gen_slice_test "slice_test3" "[2:]"
       (Call(Name("slice", dummy_uid),
             [
               Num(Int(Pos), dummy_uid);
               Name("None", dummy_uid);
               Name("None", dummy_uid);
             ],
             dummy_uid)));
    (gen_slice_test "slice_test4" "[:4]"
       (Call(Name("slice", dummy_uid),
             [
               Name("None", dummy_uid);
               Num(Int(Pos), dummy_uid);
               Name("None", dummy_uid);
             ],
             dummy_uid)));
    (gen_slice_test "slice_test5" "[::3]"
       (Call(Name("slice", dummy_uid),
             [
               Name("None", dummy_uid);
               Name("None", dummy_uid);
               Num(Int(Pos), dummy_uid);
             ],
             dummy_uid)));
    (gen_slice_test "slice_test6" "[2:4]"
       (Call(Name("slice", dummy_uid),
             [
               Num(Int(Pos), dummy_uid);
               Num(Int(Pos), dummy_uid);
               Name("None", dummy_uid);
             ],
             dummy_uid)));
    (gen_slice_test "slice_test7" "[2:4:-1]"
       (Call(Name("slice", dummy_uid),
             [
               Num(Int(Pos), dummy_uid);
               Num(Int(Pos), dummy_uid);
               Num(Int(Neg), dummy_uid);
             ],
             dummy_uid)));
  ]

(* Tests of various binary operations *)
let gen_binop_test (name : string) (prog : string) (lhs : 'a expr) (rhs : 'a expr) op =
  gen_stmt_test name prog (BinOp(lhs, op, rhs, dummy_uid))
;;

let binop_tests =
  [
    (gen_binop_test "add_int_test" "42 + 9001"
       (Num(Int(Pos), dummy_uid)) (Num(Int(Pos), dummy_uid)) Add);
    (gen_binop_test "add_float_test" "42.0 + 9001.75"
       (Num(Float(Pos), dummy_uid)) (Num(Float(Pos), dummy_uid)) Add);
    (gen_binop_test "add_int_float_test" "42 + -9001.5"
       (Num(Int(Pos), dummy_uid)) (Num(Float(Neg), dummy_uid)) Add);

    (gen_binop_test "add_str_test" "'foo' + 'bar'"
       (Str(StringLiteral("foo"), dummy_uid)) (Str(StringLiteral("bar"), dummy_uid)) Add);
    (gen_binop_test "add_int_str_test" "42 + 'foo'"
       (Num(Int(Pos), dummy_uid)) (Str(StringLiteral("foo"), dummy_uid)) Add);

    (gen_binop_test "sub_int_test" "42 - 9001"
       (Num(Int(Pos), dummy_uid)) (Num(Int(Pos), dummy_uid)) Sub);
    (gen_binop_test "mult_int_test" "42 * 9001"
       (Num(Int(Pos), dummy_uid)) (Num(Int(Pos), dummy_uid)) Mult);
    (gen_binop_test "div_int_test" "42 / 9001"
       (Num(Int(Pos), dummy_uid)) (Num(Int(Pos), dummy_uid)) Div);
    (gen_binop_test "mod_int_test" "42 % 9001"
       (Num(Int(Pos), dummy_uid)) (Num(Int(Pos), dummy_uid)) Mod);
    (gen_binop_test "pow_int_test" "42 ** 9001"
       (Num(Int(Pos), dummy_uid)) (Num(Int(Pos), dummy_uid)) Pow);

    (gen_binop_test "triple_binop_test" "(42 - 9001) + 17"
       (BinOp(Num(Int(Pos), dummy_uid),
              Sub,
              Num(Int(Pos), dummy_uid),
              dummy_uid))
       (Num(Int(Pos), dummy_uid))
       Add);

    (gen_binop_test "order_of_operations_test" "1+2*3"
       (Num(Int(Pos), dummy_uid))
       (BinOp(Num(Int(Pos), dummy_uid),
              Mult,
              Num(Int(Pos), dummy_uid),
              dummy_uid))
       Add);
  ]
  (* Run the tests *)
  *)
let tests =
  "abstract_ast">:::
  [
    int_test;
    float_test;
    float_zero_test;
    unop_test;
    unop_not_test;
    boolop_and_test;
    boolop_or_test;
    (* boolop_all_test; *)
    var_assign_test;
    var_double_assign_test;
    (* var_assign_from_tuple_test; *)
    assign_to_index_test;
    assign_to_slice_test;
    assign_to_attribute_test;
    var_aug_assign_test;
    var_cmp_test;
    funcdef_test;
    call_test;
    attribute_test;
    attribute_call_test;
    if_test;
    print_test;
    tuple_test;
    while_test;
    for_test;
    break_test;
    continue_test;
    bad_break_test;
    bad_continue_test;
    (*raise_test_no_args;
      raise_test_one_arg;
      raise_test_two_args;
      try_test;*)
    big_test;
  ]
(*@ binop_tests
  @ list_tests*)
