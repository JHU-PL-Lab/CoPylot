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
    [SimpleExprStmt(Num(Int(Pos), dummy_uid, None), dummy_uid, None)]
;;

let float_test = gen_module_test "float_test"
    "1.7"
    [SimpleExprStmt(Num(Float(Pos), dummy_uid, None), dummy_uid, None)]
;;

let float_zero_test = gen_module_test "float_zero_test"
    "0.0"
    [SimpleExprStmt(Num(Float(Zero), dummy_uid, None), dummy_uid, None)]
;;

let unop_test = gen_module_test "unop_test"
    "+4"
    [
      Assign("$normalized_unique_name_0",
             UnaryOp(UAdd, Num(Int(Pos), dummy_uid, None),
                     dummy_uid, None),
             dummy_uid, None);
      SimpleExprStmt(Name("$normalized_unique_name_0", dummy_uid, None),
                     dummy_uid, None);
    ]

;;

let unop_not_test = gen_module_test "unop_not_test"
    "not x"
    [
      Assign("$normalized_unique_name_0",
             UnaryOp(Not, Name("x", dummy_uid, None), dummy_uid, None),
             dummy_uid, None);
      SimpleExprStmt(Name("$normalized_unique_name_0", dummy_uid, None), dummy_uid, None);
    ]
;;

let boolop_and_test = gen_module_test "boolop_and_test"
    "1+2 and x and -5"
    [
      Assign("$normalized_unique_name_0",
             BinOp(
               Num(Int(Pos), dummy_uid, None),
               Add,
               Num(Int(Pos), dummy_uid, None),
               dummy_uid, None),
             dummy_uid, None);

      If(Name("$normalized_unique_name_0", dummy_uid, None),
         [
           Assign("$normalized_unique_name_1",
                  SimpleExpr(Name("x", dummy_uid, None), dummy_uid, None),
                  dummy_uid, None);
         ],
         [
           Assign("$normalized_unique_name_1",
                  SimpleExpr(Bool(false, dummy_uid, None), dummy_uid, None),
                  dummy_uid, None);
         ],
         dummy_uid, None);

      Assign("$normalized_unique_name_2",
             BoolOp(
               Name("$normalized_unique_name_0", dummy_uid, None),
               And,
               Name("$normalized_unique_name_1", dummy_uid, None),
               dummy_uid, None),
             dummy_uid, None);

      If(Name("$normalized_unique_name_2", dummy_uid, None),
         [
           Assign("$normalized_unique_name_3",
                  SimpleExpr(Num(Int(Neg), dummy_uid, None), dummy_uid, None),
                  dummy_uid, None);
         ],
         [
           Assign("$normalized_unique_name_3",
                  SimpleExpr(Bool(false, dummy_uid, None), dummy_uid, None),
                  dummy_uid, None);
         ],
         dummy_uid, None);

      Assign("$normalized_unique_name_4",
             BoolOp(
               Name("$normalized_unique_name_2", dummy_uid, None),
               And,
               Name("$normalized_unique_name_3", dummy_uid, None),
               dummy_uid, None),
             dummy_uid, None);

      SimpleExprStmt(Name("$normalized_unique_name_4", dummy_uid, None), dummy_uid, None);
    ]
;;

let boolop_or_test = gen_module_test "boolop_or_test"
    "x or False or 0"
    [
      If(Name("x", dummy_uid, None),
         [
           Assign("$normalized_unique_name_0",
                  SimpleExpr(Bool(true, dummy_uid, None), dummy_uid, None),
                  dummy_uid, None);
         ],
         [
           Assign("$normalized_unique_name_0",
                  SimpleExpr(Bool(false, dummy_uid, None), dummy_uid, None),
                  dummy_uid, None);
         ],
         dummy_uid, None);

      Assign("$normalized_unique_name_1",
             BoolOp(
               Name("x", dummy_uid, None),
               Or,
               Name("$normalized_unique_name_0", dummy_uid, None),
               dummy_uid, None),
             dummy_uid, None);

      If(Name("$normalized_unique_name_1", dummy_uid, None),
         [
           Assign("$normalized_unique_name_2",
                  SimpleExpr(Bool(true, dummy_uid, None), dummy_uid, None),
                  dummy_uid, None);
         ],
         [
           Assign("$normalized_unique_name_2",
                  SimpleExpr(Num(Int(Zero), dummy_uid, None), dummy_uid, None),
                  dummy_uid, None);
         ],
         dummy_uid, None);

      Assign("$normalized_unique_name_3",
             BoolOp(
               Name("$normalized_unique_name_1", dummy_uid, None),
               Or,
               Name("$normalized_unique_name_2", dummy_uid, None),
               dummy_uid, None),
             dummy_uid, None);

      SimpleExprStmt(Name("$normalized_unique_name_3", dummy_uid, None), dummy_uid, None);
    ]
;;

let var_assign_test = gen_module_test "var_assign_test"
    "x = 5"
    [
      Assign(
        "$simplified_unique_name_0",
        SimpleExpr(Num(Int(Pos), dummy_uid, None), dummy_uid, None),
        dummy_uid, None);

      Assign(
        "x",
        SimpleExpr(Name("$simplified_unique_name_0", dummy_uid, None), dummy_uid, None),
        dummy_uid, None);
    ]
;;

let var_double_assign_test = gen_module_test "var_double_assign_test"
    "x = y = 5"
    [
      Assign(
        "$simplified_unique_name_0",
        SimpleExpr(Num(Int(Pos), dummy_uid, None), dummy_uid, None),
        dummy_uid, None);

      Assign(
        "x",
        SimpleExpr(Name("$simplified_unique_name_0", dummy_uid, None), dummy_uid, None),
        dummy_uid, None);

      Assign(
        "y",
        SimpleExpr(Name("$simplified_unique_name_0", dummy_uid, None), dummy_uid, None),
        dummy_uid, None);
    ]
;;

let var_assign_from_tuple_test = gen_module_test "var_assign_from_tuple_test"
    "i, j = (-1,f(-1))"
    [
      Assign(
        "$normalized_unique_name_0",
        Call(
          Name("f", dummy_uid, None),
          [Num(Int(Neg), dummy_uid, None)],
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_1",
        Tuple(
          [
            Num(Int(Neg), dummy_uid, None);
            Name("$normalized_unique_name_0", dummy_uid, None);
          ],
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$simplified_unique_name_0",
        SimpleExpr(Name("$normalized_unique_name_1", dummy_uid, None),
                   dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_2",
        Attribute(
          Name("$simplified_unique_name_0", dummy_uid, None),
          "__iter__",
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_3",
        Call(
          Name("$normalized_unique_name_2", dummy_uid, None),
          [],
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_4",
        Attribute(
          Name("$normalized_unique_name_3", dummy_uid, None),
          "next",
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$simplified_unique_name_1",
        SimpleExpr(Name("$normalized_unique_name_4", dummy_uid, None),
                   dummy_uid, None),
        dummy_uid, None);
      (* I literally just copy-pasted the output of the program here. So
         this one is more of a regression test. *)
        (Assign ("$normalized_unique_name_5",
          (Call (
             (Name ("$simplified_unique_name_1", dummy_uid,
                (Some dummy_uid))),
             [], dummy_uid, (Some dummy_uid))),
          dummy_uid, (Some dummy_uid)));
       (Assign ("$simplified_unique_name_2",
          (SimpleExpr (
             (Name ("$normalized_unique_name_5", dummy_uid,
                (Some dummy_uid))),
             dummy_uid, (Some dummy_uid))),
          dummy_uid, (Some dummy_uid)));
       (Assign ("$normalized_unique_name_6",
          (Call (
             (Name ("$simplified_unique_name_1", dummy_uid,
                (Some dummy_uid))),
             [], dummy_uid, (Some dummy_uid))),
          dummy_uid, (Some dummy_uid)));
       (Assign ("$simplified_unique_name_3",
          (SimpleExpr (
             (Name ("$normalized_unique_name_6", dummy_uid,
                (Some dummy_uid))),
             dummy_uid, (Some dummy_uid))),
          dummy_uid, (Some dummy_uid)));
       (Assign ("$normalized_unique_name_7",
          (Call (
             (Name ("$simplified_unique_name_1", dummy_uid,
                (Some dummy_uid))),
             [], dummy_uid, (Some dummy_uid))),
          dummy_uid, (Some dummy_uid)));
       (SimpleExprStmt (
          (Name ("$normalized_unique_name_7", dummy_uid,
             (Some dummy_uid))),
          dummy_uid, (Some dummy_uid)));
       (Assign ("$normalized_unique_name_8",
          (Call (
             (Name ("ValueError", dummy_uid, (Some dummy_uid))),
             [(Str (
                 (StringLiteral
                    "too many values to unpack"),
                 dummy_uid, (Some dummy_uid)))
               ],
             dummy_uid, (Some dummy_uid))),
          dummy_uid, (Some dummy_uid)));
       (Raise (
          (Name ("$normalized_unique_name_8", dummy_uid,
             (Some dummy_uid))),
          dummy_uid, (Some dummy_uid)));
       (Goto (dummy_uid, dummy_uid, (Some dummy_uid)));
       (Catch ("$normalized_unique_name_9", dummy_uid, (Some dummy_uid)
          ));
       (Assign ("$normalized_unique_name_10",
          (Call (
             (Name ("type", dummy_uid, (Some dummy_uid))),
             [(Name ("$normalized_unique_name_9", dummy_uid,
                 (Some dummy_uid)))
               ],
             dummy_uid, (Some dummy_uid))),
          dummy_uid, (Some dummy_uid)));
       (If (
          (Bool (true, dummy_uid, (Some dummy_uid))),
          [(Assign ("$normalized_unique_name_11",
              (Compare (
                 (Name ("$normalized_unique_name_10", dummy_uid,
                    (Some dummy_uid))),
                 Eq,
                 (Name ("StopIteration", dummy_uid, (Some dummy_uid))),
                 dummy_uid, (Some dummy_uid))),
              dummy_uid, (Some dummy_uid)))
            ],
          [(Assign ("$normalized_unique_name_11",
              (SimpleExpr (
                 (Bool (false, dummy_uid, (Some dummy_uid))), dummy_uid,
                 (Some dummy_uid))),
              dummy_uid, (Some dummy_uid)))
            ],
          dummy_uid, (Some dummy_uid)));
       (Assign ("$normalized_unique_name_12",
          (BoolOp (
             (Bool (true, dummy_uid, (Some dummy_uid))),
             And,
             (Name ("$normalized_unique_name_11", dummy_uid,
                (Some dummy_uid))),
             dummy_uid, (Some dummy_uid))),
          dummy_uid, (Some dummy_uid)));
       (If (
          (Name ("$normalized_unique_name_12", dummy_uid,
             (Some dummy_uid))),
          [(Pass (dummy_uid, (Some dummy_uid)))],
          [(Raise (
              (Name ("$normalized_unique_name_9", dummy_uid,
                 (Some dummy_uid))),
              dummy_uid, (Some dummy_uid)))
            ],
          dummy_uid, (Some dummy_uid)));
       (Pass (dummy_uid, (Some dummy_uid)));
       (Goto (dummy_uid, dummy_uid, None));
       (Catch ("$normalized_unique_name_13", dummy_uid, None));
       (Assign ("$normalized_unique_name_14",
          (Call (
             (Name ("type", dummy_uid, None)),
             [(Name ("$normalized_unique_name_13", dummy_uid,
                 None))
               ],
             dummy_uid, None)),
          dummy_uid, None));
       (If (
          (Bool (true, dummy_uid, None)),
          [(Assign ("$normalized_unique_name_15",
              (Compare (
                 (Name ("$normalized_unique_name_14", dummy_uid,
                    None)),
                 Eq,
                 (Name ("StopIteration", dummy_uid, None)), dummy_uid,
                 None)),
              dummy_uid, None))
            ],
          [(Assign ("$normalized_unique_name_15",
              (SimpleExpr (
                 (Bool (false, dummy_uid, None)), dummy_uid, None)),
              dummy_uid, None))
            ],
          dummy_uid, None));
       (Assign ("$normalized_unique_name_16",
          (BoolOp (
             (Bool (true, dummy_uid, None)),
             And,
             (Name ("$normalized_unique_name_15", dummy_uid,
                None)),
             dummy_uid, None)),
          dummy_uid, None));
       (If (
          (Name ("$normalized_unique_name_16", dummy_uid, None)),
          [(Assign ("$normalized_unique_name_17",
              (Call (
                 (Name ("ValueError", dummy_uid, None)),
                 [(Str (
                     StringAbstract, dummy_uid, None))
                   ],
                 dummy_uid, None)),
              dummy_uid, None));
            (Raise (
               (Name ("$normalized_unique_name_17", dummy_uid,
                  None)),
               dummy_uid, None))
            ],
          [(Raise (
              (Name ("$normalized_unique_name_13", dummy_uid,
                 None)),
              dummy_uid, None))
            ],
          dummy_uid, None));
       (Pass (dummy_uid, None));
      Assign(
        "$simplified_unique_name_4",
        SimpleExpr(Name("$simplified_unique_name_2", dummy_uid, None),
                   dummy_uid, None),
        dummy_uid, None);

      Assign(
        "i",
        SimpleExpr(Name("$simplified_unique_name_4", dummy_uid, None),
                   dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$simplified_unique_name_5",
        SimpleExpr(Name("$simplified_unique_name_3", dummy_uid, None),
                   dummy_uid, None),
        dummy_uid, None);

      Assign(
        "j",
        SimpleExpr(Name("$simplified_unique_name_5", dummy_uid, None),
                   dummy_uid, None),
        dummy_uid, None);
    ]
;;

let assign_to_index_test = gen_module_test "assign_to_index_test"
    "list[1+2] = 3"
    [
      Assign(
        "$simplified_unique_name_0",
        SimpleExpr(Num(Int(Pos), dummy_uid, None), dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_0",
        Attribute(
          Name("list", dummy_uid, None),
          "__setitem__",
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_1",
        BinOp(
          Num(Int(Pos), dummy_uid, None),
          Add,
          Num(Int(Pos), dummy_uid, None),
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_2",
        Call(
          Name("$normalized_unique_name_0", dummy_uid, None),
          [
            Name("$normalized_unique_name_1", dummy_uid, None);
            Name("$simplified_unique_name_0", dummy_uid, None);
          ],
          dummy_uid, None),
        dummy_uid, None);

      SimpleExprStmt(
        Name("$normalized_unique_name_2", dummy_uid, None),
        dummy_uid, None);
    ]
;;

let assign_to_slice_test = gen_module_test "assign_to_slice_test"
    "list[1:2] = 3"
    [
      Assign(
        "$simplified_unique_name_0",
        SimpleExpr(Num(Int(Pos), dummy_uid, None), dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_0",
        Attribute(
          Name("list", dummy_uid, None),
          "__setitem__",
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_1",
        Call(
          Name("slice", dummy_uid, None),
          [
            Num(Int(Pos), dummy_uid, None);
            Num(Int(Pos), dummy_uid, None);
            Name("None", dummy_uid, None);
          ],
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_2",
        Call(
          Name("$normalized_unique_name_0", dummy_uid, None),
          [
            Name("$normalized_unique_name_1", dummy_uid, None);
            Name("$simplified_unique_name_0", dummy_uid, None);
          ],
          dummy_uid, None),
        dummy_uid, None);

      SimpleExprStmt(
        Name("$normalized_unique_name_2", dummy_uid, None),
        dummy_uid, None);
    ]
;;

let assign_to_attribute_test = gen_module_test "assign_to_attribute_test"
    "obj.member = 7"
    [
      Assign(
        "$simplified_unique_name_0",
        SimpleExpr(Num(Int(Pos), dummy_uid, None), dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_0",
        Attribute(
          Name("obj", dummy_uid, None),
          "__setattr__",
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_1",
        Call(
          Name("$normalized_unique_name_0", dummy_uid, None),
          [
            Str(StringLiteral("member"), dummy_uid, None);
            Name("$simplified_unique_name_0", dummy_uid, None);
          ],
          dummy_uid, None),
        dummy_uid, None);

      SimpleExprStmt(Name("$normalized_unique_name_1", dummy_uid, None),
                     dummy_uid, None);

    ]
;;

let var_aug_assign_test = gen_module_test "var_aug_assign_test"
    "x *= -5"
    [
      Assign(
        "$normalized_unique_name_0",
        BinOp(Name("x", dummy_uid, None),
              Mult,
              Num(Int(Neg), dummy_uid, None),
              dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$simplified_unique_name_0",
        SimpleExpr(Name("$normalized_unique_name_0", dummy_uid, None), dummy_uid, None),
        dummy_uid, None);

      Assign(
        "x",
        SimpleExpr(Name("$simplified_unique_name_0", dummy_uid, None), dummy_uid, None),
        dummy_uid, None)
    ]
;;

let var_cmp_test = gen_module_test "var_cmp_test"
    "x <= 9000+1 > True"
    [
      If(Bool(true, dummy_uid, None),
         [
           Assign(
             "$normalized_unique_name_0",
             BinOp(
               Num(Int(Pos), dummy_uid, None),
               Add,
               Num(Int(Pos), dummy_uid, None),
               dummy_uid, None),
             dummy_uid, None);

           Assign(
             "$normalized_unique_name_1",
             Compare(
               Name("x", dummy_uid, None),
               LtE,
               Name("$normalized_unique_name_0", dummy_uid, None),
               dummy_uid, None),
             dummy_uid, None)
         ],
         [
           Assign(
             "$normalized_unique_name_1",
             SimpleExpr(Bool(false, dummy_uid, None), dummy_uid, None),
             dummy_uid, None)
         ],
         dummy_uid, None);

      Assign(
        "$normalized_unique_name_2",
        BoolOp(
          Bool(true, dummy_uid, None),
          And,
          Name("$normalized_unique_name_1", dummy_uid, None),
          dummy_uid, None),
        dummy_uid, None);

      If(
        Name("$normalized_unique_name_2", dummy_uid, None),
        [
          Assign(
            "$normalized_unique_name_3",
            Compare(
              Name("$normalized_unique_name_0", dummy_uid, None),
              Gt,
              Bool(true, dummy_uid, None),
              dummy_uid, None),
            dummy_uid, None)
        ],
        [
          Assign(
            "$normalized_unique_name_3",
            SimpleExpr(Bool(false, dummy_uid, None), dummy_uid, None),
            dummy_uid, None)
        ],
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_4",
        BoolOp(
          Name("$normalized_unique_name_2", dummy_uid, None),
          And,
          Name("$normalized_unique_name_3", dummy_uid, None),
          dummy_uid, None),
        dummy_uid, None);

      SimpleExprStmt(
        Name("$normalized_unique_name_4", dummy_uid, None),
        dummy_uid, None);
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
                    Return(Some(Name("arg1", dummy_uid, None)),
                           dummy_uid, None)
                  ],
                  dummy_uid, None)
    ]
;;

let call_test = gen_module_test "call_test"
    "func(1,x-1,get_arg('foo'))"
    [
      Assign(
        "$normalized_unique_name_0",
        BinOp(
          Name("x", dummy_uid, None),
          Sub,
          Num(Int(Pos), dummy_uid, None),
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_1",
        Call(
          Name("get_arg", dummy_uid, None),
          [Str(StringLiteral("foo"), dummy_uid, None)],
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_2",
        Call(
          Name("func", dummy_uid, None),
          [
            Num(Int(Pos), dummy_uid, None);
            Name("$normalized_unique_name_0", dummy_uid, None);
            Name("$normalized_unique_name_1", dummy_uid, None);
          ],
          dummy_uid, None),
        dummy_uid, None);

      SimpleExprStmt(
        Name("$normalized_unique_name_2", dummy_uid, None),
        dummy_uid, None);
    ]
;;

let attribute_test = gen_module_test "attribute_test"
    "obj.member_var"
    [
      Assign(
        "$normalized_unique_name_0",
        Attribute(
          Name("obj", dummy_uid, None),
          "member_var",
          dummy_uid, None),
        dummy_uid, None);

      SimpleExprStmt(
        Name("$normalized_unique_name_0", dummy_uid, None),
        dummy_uid, None);
    ]
;;

let attribute_call_test = gen_module_test "attribute_call_test"
    "obj.member_func()"
    [
      Assign(
        "$normalized_unique_name_0",
        Attribute(
          Name("obj", dummy_uid, None),
          "member_func",
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_1",
        Call(
          Name("$normalized_unique_name_0", dummy_uid, None),
          [],
          dummy_uid, None),
        dummy_uid, None);

      SimpleExprStmt(
        Name("$normalized_unique_name_1", dummy_uid, None),
        dummy_uid, None);
    ]
;;

let if_test = gen_module_test "if_test"
    "if x > 1+1:\n\tx = foo()\nelif x < 0.0: error('x < 0')\nelse: pass"
    [
      If(
        Bool(true, dummy_uid, None),
        [
          Assign(
            "$normalized_unique_name_0",
            BinOp(
              Num(Int(Pos), dummy_uid, None),
              Add,
              Num(Int(Pos), dummy_uid, None),
              dummy_uid, None),
            dummy_uid, None);

          Assign(
            "$normalized_unique_name_1",
            Compare(
              Name("x", dummy_uid, None),
              Gt,
              Name("$normalized_unique_name_0", dummy_uid, None),
              dummy_uid, None),
            dummy_uid, None);
        ],
        [
          Assign(
            "$normalized_unique_name_1",
            SimpleExpr(Bool(false, dummy_uid, None), dummy_uid, None),
            dummy_uid, None);
        ],
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_2",
        BoolOp(
          Bool(true, dummy_uid, None),
          And,
          Name("$normalized_unique_name_1", dummy_uid, None),
          dummy_uid, None),
        dummy_uid, None);

      If(
        Name("$normalized_unique_name_2", dummy_uid, None),
        [
          Assign(
            "$normalized_unique_name_3",
            Call(
              Name("foo", dummy_uid, None),
              [],
              dummy_uid, None),
            dummy_uid, None);

          Assign(
            "$simplified_unique_name_0",
            SimpleExpr(
              Name("$normalized_unique_name_3", dummy_uid, None),
              dummy_uid, None),
            dummy_uid, None);

          Assign(
            "x",
            SimpleExpr(
              Name("$simplified_unique_name_0", dummy_uid, None),
              dummy_uid, None),
            dummy_uid, None);
        ],
        [
          If( (* Computing x < 0.0 *)
            Bool(true, dummy_uid, None),
            [
              Assign(
                "$normalized_unique_name_4",
                Compare(
                  Name("x", dummy_uid, None),
                  Lt,
                  Num(Float(Zero), dummy_uid, None),
                  dummy_uid, None),
                dummy_uid, None);
            ],
            [
              Assign(
                "$normalized_unique_name_4",
                SimpleExpr(Bool(false, dummy_uid, None), dummy_uid, None),
                dummy_uid, None);
            ],
            dummy_uid, None);

          Assign(
            "$normalized_unique_name_5",
            BoolOp(
              Bool(true, dummy_uid, None),
              And,
              Name("$normalized_unique_name_4", dummy_uid, None),
              dummy_uid, None),
            dummy_uid, None);

          If( (* Elif stmt *)
            Name("$normalized_unique_name_5", dummy_uid, None),
            [
              Assign(
                "$normalized_unique_name_6",
                Call(
                  Name("error", dummy_uid, None),
                  [Str(StringLiteral("x < 0"), dummy_uid, None)],
                  dummy_uid, None),
                dummy_uid, None);

              SimpleExprStmt(
                Name("$normalized_unique_name_6", dummy_uid, None),
                dummy_uid, None);
            ],
            [
              Pass(dummy_uid, None);
            ],
            dummy_uid, None)
        ],
        dummy_uid, None)
    ]
;;

let print_test = gen_module_test "print_test"
    "print 1,x(1+1),'foo'"
    [
      Assign(
        "$normalized_unique_name_0",
        BinOp(
          Num(Int(Pos), dummy_uid, None),
          Add,
          Num(Int(Pos), dummy_uid, None),
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_1",
        Call(
          Name("x", dummy_uid, None),
          [Name("$normalized_unique_name_0", dummy_uid, None)],
          dummy_uid, None),
        dummy_uid, None);

      Print(None,
            [
              Num(Int(Pos), dummy_uid, None);
              Name("$normalized_unique_name_1", dummy_uid, None);
              Str(StringLiteral("foo"),dummy_uid, None);
            ],
            true,
            dummy_uid, None)
    ]
;;

let tuple_test = gen_module_test "tuple_test"
    "(1,1+1,(2,f(2)),'foo')"
    [
      Assign( (* 1+1 *)
        "$normalized_unique_name_0",
        BinOp(
          Num(Int(Pos), dummy_uid, None),
          Add,
          Num(Int(Pos), dummy_uid, None),
          dummy_uid, None),
        dummy_uid, None);

      Assign( (* f(2) *)
        "$normalized_unique_name_1",
        Call(
          Name("f", dummy_uid, None),
          [Num(Int(Pos), dummy_uid, None)],
          dummy_uid, None),
        dummy_uid, None);

      Assign( (* (2, f(2)) *)
        "$normalized_unique_name_2",
        Tuple(
          [
            Num(Int(Pos), dummy_uid, None);
            Name("$normalized_unique_name_1", dummy_uid, None);
          ],
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_3",
        Tuple(
          [
            Num(Int(Pos), dummy_uid, None);
            Name("$normalized_unique_name_0", dummy_uid, None);
            Name("$normalized_unique_name_2", dummy_uid, None);
            Str(StringLiteral("foo"), dummy_uid, None);
          ],
          dummy_uid, None),
        dummy_uid, None);

      SimpleExprStmt(
        Name("$normalized_unique_name_3", dummy_uid, None),
        dummy_uid, None);
    ]
;;

let while_test = gen_module_test "while_test"
    "while x < 9001:\n\tx = x+1"
    [
      Pass(dummy_uid, None);

      If(
        Bool(true, dummy_uid, None),
        [
          Assign(
            "$normalized_unique_name_0",
            Compare(
              Name("x", dummy_uid, None),
              Lt,
              Num(Int(Pos), dummy_uid, None),
              dummy_uid, None),
            dummy_uid, None);
        ],
        [
          Assign(
            "$normalized_unique_name_0",
            SimpleExpr(Bool(false, dummy_uid, None), dummy_uid, None),
            dummy_uid, None);
        ],
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_1",
        BoolOp(
          Bool(true, dummy_uid, None),
          And,
          Name("$normalized_unique_name_0", dummy_uid, None),
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_2",
        UnaryOp(
          Not,
          Name("$normalized_unique_name_1", dummy_uid, None),
          dummy_uid, None),
        dummy_uid, None);

      If(
        Name("$normalized_unique_name_2", dummy_uid, None),
        [
          Goto(dummy_uid, dummy_uid, None);
        ],
        [],
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_3",
        BinOp(Name("x", dummy_uid, None),
              Add,
              Num(Int(Pos), dummy_uid, None),
              dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$simplified_unique_name_0",
        SimpleExpr(
          Name("$normalized_unique_name_3", dummy_uid, None),
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "x",
        SimpleExpr(
          Name("$simplified_unique_name_0", dummy_uid, None),
          dummy_uid, None),
        dummy_uid, None);

      Goto(dummy_uid, dummy_uid, None);

      Pass(dummy_uid, None);
    ]
;;

let break_test = gen_module_test "break_test"
    "while x < 9001:\n\tbreak"
    [
      Pass(dummy_uid, None);

      If(
        Bool(true, dummy_uid, None),
        [
          Assign(
            "$normalized_unique_name_0",
            Compare(
              Name("x", dummy_uid, None),
              Lt,
              Num(Int(Pos), dummy_uid, None),
              dummy_uid, None),
            dummy_uid, None);
        ],
        [
          Assign(
            "$normalized_unique_name_0",
            SimpleExpr(Bool(false, dummy_uid, None), dummy_uid, None),
            dummy_uid, None);
        ],
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_1",
        BoolOp(
          Bool(true, dummy_uid, None),
          And,
          Name("$normalized_unique_name_0", dummy_uid, None),
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_2",
        UnaryOp(
          Not,
          Name("$normalized_unique_name_1", dummy_uid, None),
          dummy_uid, None),
        dummy_uid, None);

      If(
        Name("$normalized_unique_name_2", dummy_uid, None),
        [
          Goto(dummy_uid, dummy_uid, None);
        ],
        [],
        dummy_uid, None);

      Goto(dummy_uid, dummy_uid, None);

      Goto(dummy_uid, dummy_uid, None);

      Pass(dummy_uid, None);
    ]
;;

let continue_test = gen_module_test "continue_test"
    "while x < 9001:\n\tcontinue"
    [
      Pass(dummy_uid, None);

      If(
        Bool(true, dummy_uid, None),
        [
          Assign(
            "$normalized_unique_name_0",
            Compare(
              Name("x", dummy_uid, None),
              Lt,
              Num(Int(Pos), dummy_uid, None),
              dummy_uid, None),
            dummy_uid, None);
        ],
        [
          Assign(
            "$normalized_unique_name_0",
            SimpleExpr(Bool(false, dummy_uid, None), dummy_uid, None),
            dummy_uid, None);
        ],
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_1",
        BoolOp(
          Bool(true, dummy_uid, None),
          And,
          Name("$normalized_unique_name_0", dummy_uid, None),
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_2",
        UnaryOp(
          Not,
          Name("$normalized_unique_name_1", dummy_uid, None),
          dummy_uid, None),
        dummy_uid, None);

      If(
        Name("$normalized_unique_name_2", dummy_uid, None),
        [
          Goto(dummy_uid, dummy_uid, None);
        ],
        [],
        dummy_uid, None);

      Goto(dummy_uid, dummy_uid, None);

      Goto(dummy_uid, dummy_uid, None);

      Pass(dummy_uid, None);
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

let raise_test = gen_module_test "raise_test"
    "raise ValueError"
    [Raise(
        Name("ValueError", dummy_uid, None),
        dummy_uid, None)]
;;


let try_block =
  "try:" ^
  "\n\tx = 5" ^
  "\nexcept ValueError:" ^
  "\n\tprint 'Error:', get_error()" ^
  "\nexcept StopIteration as e:" ^
  "\n\tprint e" ^
  "\n"
;;

let try_test = gen_module_test "try_test"
    try_block
    [
      (* Try body *)
      Assign(
        "$simplified_unique_name_0",
        SimpleExpr(Num(Int(Pos), dummy_uid, Some(dummy_uid)),
                   dummy_uid, Some(dummy_uid)),
        dummy_uid, Some(dummy_uid));

      Assign(
        "x",
        SimpleExpr(Name("$simplified_unique_name_0",
                        dummy_uid, Some(dummy_uid)),
                   dummy_uid, Some(dummy_uid)),
        dummy_uid, Some(dummy_uid));

      Goto(dummy_uid, dummy_uid, None);

      (* Catch exception *)
      Catch("$normalized_unique_name_0", dummy_uid, None);

      (* Handler 1 - construct test expression*)
      Assign(
        "$normalized_unique_name_1",
        Call(
          Name("type", dummy_uid, None),
          [Name("$normalized_unique_name_0", dummy_uid, None)],
          dummy_uid, None),
        dummy_uid, None);

      If(
        Bool(true, dummy_uid, None),
        [
          Assign(
            "$normalized_unique_name_2",
            Compare(
              Name("$normalized_unique_name_1", dummy_uid, None),
              Eq,
              Name("ValueError", dummy_uid, None),
              dummy_uid, None),
            dummy_uid, None);
        ],
        [
          Assign(
            "$normalized_unique_name_2",
            SimpleExpr(Bool(false, dummy_uid, None),
                       dummy_uid, None),
            dummy_uid, None);
        ],
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_3",
        BoolOp(
          Bool(true, dummy_uid, None),
          And,
          Name("$normalized_unique_name_2", dummy_uid, None),
          dummy_uid, None),
        dummy_uid, None);

      (* Handler body *)
      If(
        Name("$normalized_unique_name_3", dummy_uid, None),
        [
          Assign(
            "$normalized_unique_name_4",
            Call(
              Name("get_error", dummy_uid, None),
              [],
              dummy_uid, None),
            dummy_uid, None);

          Print(
            None,
            [
              Str(StringLiteral("Error:"), dummy_uid, None);
              Name("$normalized_unique_name_4", dummy_uid, None);
            ],
            true,
            dummy_uid, None)
        ],
        [
          (* Construct handler 2 test *)
          Assign(
            "$normalized_unique_name_5",
            Call(
              Name("type", dummy_uid, None),
              [Name("$normalized_unique_name_0", dummy_uid, None)],
              dummy_uid, None),
            dummy_uid, None);

          If(
            Bool(true, dummy_uid, None),
            [
              Assign(
                "$normalized_unique_name_6",
                Compare(
                  Name("$normalized_unique_name_5", dummy_uid, None),
                  Eq,
                  Name("StopIteration", dummy_uid, None),
                  dummy_uid, None),
                dummy_uid, None);
            ],
            [
              Assign(
                "$normalized_unique_name_6",
                SimpleExpr(Bool(false, dummy_uid, None),
                           dummy_uid, None),
                dummy_uid, None);
            ],
            dummy_uid, None);

          Assign(
            "$normalized_unique_name_7",
            BoolOp(
              Bool(true, dummy_uid, None),
              And,
              Name("$normalized_unique_name_6", dummy_uid, None),
              dummy_uid, None),
            dummy_uid, None);

          (* Handler 2 body *)
          If(
            Name("$normalized_unique_name_7", dummy_uid, None),
            [
              Assign(
                "e",
                SimpleExpr(
                  Name("$normalized_unique_name_0", dummy_uid, None),
                  dummy_uid, None),
                dummy_uid, None);

              Print (None,
                     [Name("e",dummy_uid, None)],
                     true,
                     dummy_uid, None)
            ],
            [
              Raise(Name("$normalized_unique_name_0", dummy_uid, None),
                    dummy_uid, None);
            ],
            dummy_uid, None);
        ],
        dummy_uid, None);

      Pass(dummy_uid, None);
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
  FunctionDef(
    "triangle",
    ["n"],
    [ (* Body *)
      Assign(
        "$simplified_unique_name_0",
        SimpleExpr(Num(Int(Zero), dummy_uid, None), dummy_uid, None),
        dummy_uid, None);

      Assign(
        "count",
        SimpleExpr(Name("$simplified_unique_name_0", dummy_uid, None),
                   dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$simplified_unique_name_1",
        SimpleExpr(Num(Int(Zero), dummy_uid, None), dummy_uid, None),
        dummy_uid, None);

      Assign(
        "i",
        SimpleExpr(Name("$simplified_unique_name_1", dummy_uid, None),
                   dummy_uid, None),
        dummy_uid, None);

      Pass(dummy_uid, None);

      If(
        Bool(true, dummy_uid, None),
        [
          Assign(
            "$normalized_unique_name_0",
            Compare(
              Name("count", dummy_uid, None),
              Lt,
              Name("n", dummy_uid, None),
              dummy_uid, None),
            dummy_uid, None);
        ],
        [
          Assign(
            "$normalized_unique_name_0",
            SimpleExpr(Bool(false, dummy_uid, None), dummy_uid, None),
            dummy_uid, None);
        ],
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_1",
        BoolOp(
          Bool(true, dummy_uid, None),
          And,
          Name("$normalized_unique_name_0", dummy_uid, None),
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_2",
        UnaryOp(
          Not,
          Name("$normalized_unique_name_1", dummy_uid, None),
          dummy_uid, None),
        dummy_uid, None);

      If(
        Name("$normalized_unique_name_2", dummy_uid, None),
        [
          Goto(dummy_uid, dummy_uid, None);
        ],
        [],
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_3",
        BinOp(Name("i", dummy_uid, None),
              Add,
              Name("count", dummy_uid, None),
              dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$simplified_unique_name_2",
        SimpleExpr(Name("$normalized_unique_name_3", dummy_uid, None),
                   dummy_uid, None),
        dummy_uid, None);

      Assign(
        "i",
        SimpleExpr(
          Name("$simplified_unique_name_2", dummy_uid, None),
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_4",
        BinOp(Name("count", dummy_uid, None),
              Add,
              Num(Int(Pos), dummy_uid, None),
              dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$simplified_unique_name_3",
        SimpleExpr(Name("$normalized_unique_name_4", dummy_uid, None),
                   dummy_uid, None),
        dummy_uid, None);

      Assign(
        "count",
        SimpleExpr(
          Name("$simplified_unique_name_3", dummy_uid, None),
          dummy_uid, None),
        dummy_uid, None);

      Goto(dummy_uid, dummy_uid, None);

      Pass(dummy_uid, None);

      Return(Some(Name("i", dummy_uid, None)), dummy_uid, None);
    ],
    dummy_uid, None)
;;

let big_test = gen_module_test "big_test"
    (triangle_def ^ "\n[triangle(1),triangle(7)]")
    [
      triangle_ast;

      Assign(
        "$normalized_unique_name_5",
        Call(
          Name("triangle", dummy_uid, None),
          [Num(Int(Pos), dummy_uid, None)],
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_6",
        Call(
          Name("triangle", dummy_uid, None),
          [Num(Int(Pos), dummy_uid, None)],
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_7",
        List(
          [
            Name("$normalized_unique_name_5", dummy_uid, None);
            Name("$normalized_unique_name_6", dummy_uid, None);
          ],
          dummy_uid, None),
        dummy_uid, None);

      SimpleExprStmt(
        Name("$normalized_unique_name_7", dummy_uid, None),
        dummy_uid, None);
    ]
;;

(* Tests of lists and slicing *)
let list_str = "[1,2,f(3),'four','five',2+4]";;
let list_expr =
  [
    Assign(
      "$normalized_unique_name_0",
      Call(
        Name("f", dummy_uid, None),
        [Num(Int(Pos), dummy_uid, None)],
        dummy_uid, None),
      dummy_uid, None);

    Assign(
      "$normalized_unique_name_1",
      BinOp(
        Num(Int(Pos), dummy_uid, None),
        Add,
        Num(Int(Pos), dummy_uid, None),
        dummy_uid, None),
      dummy_uid, None);

    Assign(
      "$normalized_unique_name_2",
      List(
        [
          Num(Int(Pos),dummy_uid, None);
          Num(Int(Pos),dummy_uid, None);
          Name("$normalized_unique_name_0", dummy_uid, None);
          Str(StringLiteral("four"), dummy_uid, None);
          Str(StringLiteral("five"), dummy_uid, None);
          Name("$normalized_unique_name_1", dummy_uid, None);
        ],
        dummy_uid, None),
      dummy_uid, None);
  ]

let list_test = gen_module_test "list_test"
    list_str
    [
      Assign(
        "$normalized_unique_name_0",
        Call(
          Name("f", dummy_uid, None),
          [Num(Int(Pos), dummy_uid, None)],
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_1",
        BinOp(
          Num(Int(Pos), dummy_uid, None),
          Add,
          Num(Int(Pos), dummy_uid, None),
          dummy_uid, None),
        dummy_uid, None);

      Assign(
        "$normalized_unique_name_2",
        List(
          [
            Num(Int(Pos),dummy_uid, None);
            Num(Int(Pos),dummy_uid, None);
            Name("$normalized_unique_name_0", dummy_uid, None);
            Str(StringLiteral("four"), dummy_uid, None);
            Str(StringLiteral("five"), dummy_uid, None);
            Name("$normalized_unique_name_1", dummy_uid, None);
          ],
          dummy_uid, None),
        dummy_uid, None);

      SimpleExprStmt(Name("$normalized_unique_name_2", dummy_uid, None),
                     dummy_uid, None)
    ]
;;

(* Tests of various binary operations *)
let gen_binop_test (name : string) (prog : string)
    (lhs : simple_expr) (rhs : simple_expr) op =
  gen_module_test name prog
    [
      Assign("$normalized_unique_name_0",
             BinOp(lhs, op, rhs, dummy_uid, None),
             dummy_uid, None);

      SimpleExprStmt(Name("$normalized_unique_name_0", dummy_uid, None),
                     dummy_uid, None);
    ]
;;

let binop_tests =
  [
    (gen_binop_test "add_int_test" "42 + 9001"
       (Num(Int(Pos), dummy_uid, None)) (Num(Int(Pos), dummy_uid, None)) Add);
    (gen_binop_test "add_float_test" "42.0 + 9001.75"
       (Num(Float(Pos), dummy_uid, None)) (Num(Float(Pos), dummy_uid, None)) Add);
    (gen_binop_test "add_int_float_test" "42 + -9001.5"
       (Num(Int(Pos), dummy_uid, None)) (Num(Float(Neg), dummy_uid, None)) Add);

    (gen_binop_test "add_str_test" "'foo' + 'bar'"
       (Str(StringLiteral("foo"), dummy_uid, None)) (Str(StringLiteral("bar"), dummy_uid, None)) Add);
    (gen_binop_test "add_int_str_test" "42 + 'foo'"
       (Num(Int(Pos), dummy_uid, None)) (Str(StringLiteral("foo"), dummy_uid, None)) Add);

    (gen_binop_test "sub_int_test" "42 - 9001"
       (Num(Int(Pos), dummy_uid, None)) (Num(Int(Pos), dummy_uid, None)) Sub);
    (gen_binop_test "mult_int_test" "42 * 9001"
       (Num(Int(Pos), dummy_uid, None)) (Num(Int(Pos), dummy_uid, None)) Mult);
    (gen_binop_test "div_int_test" "42 / 9001"
       (Num(Int(Pos), dummy_uid, None)) (Num(Int(Pos), dummy_uid, None)) Div);
    (gen_binop_test "mod_int_test" "42 % 9001"
       (Num(Int(Pos), dummy_uid, None)) (Num(Int(Pos), dummy_uid, None)) Mod);
    (gen_binop_test "pow_int_test" "42 ** 9001"
       (Num(Int(Pos), dummy_uid, None)) (Num(Int(Pos), dummy_uid, None)) Pow);
  ]
(* Run the tests *)

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
    var_assign_test;
    var_double_assign_test;
    var_assign_from_tuple_test;
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
    break_test;
    continue_test;
    bad_break_test;
    bad_continue_test;
    raise_test;
    try_test;
    big_test;
    list_test;
  ]
  @ binop_tests
