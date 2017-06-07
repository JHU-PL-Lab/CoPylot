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

(* Return true if there are no duplicates (which is what we want) *)
let rec check_for_duplicates (lst : int list) : bool =
  let sorted = List.sort compare lst in
  match sorted with
  | [] -> true
  | head::rest ->
    let _, value =
      List.fold_left
        (fun (prev : int * bool) (next : int) ->
           next, (snd prev) && (next != fst prev))
        (head, true)
        rest
    in value

let rec verify_unique_uids = function
  | Module(body, uid) ->
    let uids = List.concat (List.map collect_uids_stmt body) in
    check_for_duplicates (uid::uids)

and collect_uids_stmt = function
  | Assign (_, value, u, _)
    -> u::(collect_uids_cexpr value)
  | FunctionDef (_, _, body, u, _)
    -> u::(List.concat (List.map collect_uids_stmt body))
  | Return (value, u, _)
    -> begin
        match value with
        | None -> [u]
        | Some(e) -> u::(collect_uids_sexpr e)
      end
  | Print (dest, values, _, u, _)
    ->
    let big_list =
      u::(List.concat (List.map collect_uids_sexpr values)) in
    begin
      match dest with
      | None -> big_list
      | Some(e) -> (collect_uids_sexpr e) @ big_list
    end
  | Raise (value, u, _)
    -> u::(collect_uids_sexpr value)
  | Catch (_, u, _)
  | Pass (u, _)
  | Goto (_, u, _)
  | GotoIfNot (_, _, u, _)
    -> [u]
  | SimpleExprStmt (value, u, _)
    -> u::(collect_uids_sexpr value)

and collect_uids_cexpr = function
  | Call (func, args, u, _)
    -> u::((collect_uids_sexpr func) @
           (List.concat (List.map collect_uids_sexpr args)))
  | Attribute (obj, _, u, _)
    -> u::(collect_uids_sexpr obj)
  | List (elts, u, _)
  | Tuple (elts, u, _)
    -> u::(List.concat (List.map collect_uids_sexpr elts))
  | SimpleExpr (value, u, _)
    -> u::(collect_uids_sexpr value)

and collect_uids_sexpr = function
  | Literal (l, u, _)
    -> u::collect_uids_literal l
  | Name (_, u, _)
    -> [u]

and collect_uids_literal = function
  | Num (_, u, _)
  | Str (_, u, _)
  | Bool (_, u, _)
  | Builtin (_, u, _)
    -> [u]
;;

let gen_module_test (name : string) (prog : string)
    (test_func : stmt list -> bool)=
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
      let distinct_uids = verify_unique_uids actual in
      match actual with
      | Module (body, _) ->
        assert_bool ("Incorrect Tree:\n" ^ string_of_modl actual)
          (test_func body);
        assert_bool ("Repeated UIDs:\n" ^ string_of_modl actual)
          distinct_uids
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

let uid_test =
  "uid_test">::
  ( fun _ ->
      let actual = Module([Assign("x", SimpleExpr(Literal(Num(Int(Zero), 1, None), 2, None), 3, None), 4, None);
                           Assign("y", SimpleExpr(Literal(Num(Int(Pos), 5, None), 6, None), 7, None), 8, None);], 0) in
      let map = Python2_uid_stmt_map.get_uid_hashtbl actual in
      assert_equal (Assign("y", SimpleExpr(Literal(Num(Int(Pos), 5, None), 6, None), 7, None), 8, None)) (Uid_hashtbl.find map 8)
  )
;;

let int_test = gen_module_test "int_test"
    "4"
    (function
      | [SimpleExprStmt(Literal(Num(Int(Pos), _, None), _, None), _, None)] -> true
      | _ -> false
    )
;;

let float_test = gen_module_test "float_test"
    "1.7"
    (function
      | [SimpleExprStmt(Literal(Num(Float(Pos), _, None), _, None), _, None)] -> true
      | _ -> false
    )
;;

let float_zero_test = gen_module_test "float_zero_test"
    "0.0"
    (function
      | [SimpleExprStmt(Literal(Num(Float(Zero), _, None), _, None), _, None)] -> true
      | _ -> false
    )
;;

let unop_test = gen_module_test "unop_test"
    "+4"
    (function
      |
        [
          Assign("$normalized_unique_name_0",
                 Attribute(Literal(Num(Int(Pos), _, None), _, None),
                           "__pos__",
                           _, None),
                 _, None);

          Assign("$normalized_unique_name_1",
                 Call(Name("$normalized_unique_name_0", _, None),
                      [],
                      _, None),
                 _, None);

          SimpleExprStmt(Name("$normalized_unique_name_0", _, None),
                         _, None);
        ] -> true
      | _ -> false)
;;

(* let unop_not_test = gen_module_test "unop_not_test"
    "not x"
    (function
      | [
        Assign("$normalized_unique_name_0",
               UnaryOp(Not, Name("x", _, None), _, None),
               _, None);
        SimpleExprStmt(Name("$normalized_unique_name_0", _, None), _, None);
      ] -> true
      | _ -> false
    )
   ;; *)

(* let boolop_and_test = gen_module_test "boolop_and_test"
    "1+2 and x and -5"
    (function
      | [
        Assign("$normalized_unique_name_0",
               Attribute(Literal(Num(Int(Pos), _, None), _, None),
                         "__add__",
                         _, None),
               _, None);

        Assign("$normalized_unique_name_1",
               Call(Name("$normalized_unique_name_0", _, None),
                    [Literal(Num(Int(Pos), _, None), _, None)],
                    _, None),
               _, None);

        GotoIfNot(Name("$normalized_unique_name_2", _, None),
                  orelse_uid1_jump,
                  _, None);

        Assign("$normalized_unique_name_3",
               Attribute(Name("x", _, None),
                         "__add__",
                         _, None),
               _, None);

        Assign("$normalized_unique_name_4",
               Call(Name("$normalized_unique_name_3", _, None),
                    [Literal(Num(Int(Pos), _, None), _, None)],
                    _, None),
               _, None);

        (* Interior if *)
        GotoIfNot(Name("$normalized_unique_name_5", _, None),
                  orelse_uid2_jump,
                  _, None);

        Assign("$normalized_unique_name_6",
               SimpleExpr(Literal(Num(Int(Neg), _, None), _, None),
                          _, None),
               _, None);

        Goto(end_uid2_jump, _, None);

        Pass(orelse_uid2, None);

        Assign("$normalized_unique_name_6",
               SimpleExpr(Name("$normalized_unique_name_5", _, None), _, None),
               _, None);

        Pass(end_uid2, None);
        (* End interior if *)

        Assign("$normalized_unique_name_7",
               SimpleExpr(Name("$normalized_unique_name_6", _, None), _, None),
               _, None);

        Goto(end_uid1_jump2, _, None);

        Pass(orelse_uid1, None);

        Assign("$")



      ] -> true
      | _ -> false
    )
;; *)

let boolop_or_test = gen_module_test "boolop_or_test"
    "x or False or 0"
    (function
      | [
        If(Name("x", _, None),
           [
             Assign("$normalized_unique_name_0",
                    SimpleExpr(Literal(Bool(true, _, None), _, None), _, None),
                    _, None);
           ],
           [
             Assign("$normalized_unique_name_0",
                    SimpleExpr(Literal(Bool(false, _, None), _, None), _, None),
                    _, None);
           ],
           _, None);

        Assign("$normalized_unique_name_1",
               BoolOp(
                 Name("x", _, None),
                 Or,
                 Name("$normalized_unique_name_0", _, None),
                 _, None),
               _, None);

        If(Name("$normalized_unique_name_1", _, None),
           [
             Assign("$normalized_unique_name_2",
                    SimpleExpr(Literal(Bool(true, _, None), _, None), _, None),
                    _, None);
           ],
           [
             Assign("$normalized_unique_name_2",
                    SimpleExpr(Literal(Num(Int(Zero), _, None), _, None), _, None),
                    _, None);
           ],
           _, None);

        Assign("$normalized_unique_name_3",
               BoolOp(
                 Name("$normalized_unique_name_1", _, None),
                 Or,
                 Name("$normalized_unique_name_2", _, None),
                 _, None),
               _, None);

        SimpleExprStmt(Name("$normalized_unique_name_3", _, None), _, None);
      ] -> true
      | _ -> false
    )
;;

let var_assign_test = gen_module_test "var_assign_test"
    "x = 5"
    (function
      | [
        Assign(
          "$simplified_unique_name_0",
          SimpleExpr(Literal(Num(Int(Pos), _, None), _, None), _, None),
          _, None);

        Assign(
          "x",
          SimpleExpr(Name("$simplified_unique_name_0", _, None), _, None),
          _, None);
      ] -> true
      | _ -> false
    )
;;

let var_double_assign_test = gen_module_test "var_double_assign_test"
    "x = y = 5"
    (function
      | [
        Assign(
          "$simplified_unique_name_0",
          SimpleExpr(Literal(Num(Int(Pos), _, None), _, None), _, None),
          _, None);

        Assign(
          "x",
          SimpleExpr(Name("$simplified_unique_name_0", _, None), _, None),
          _, None);

        Assign(
          "y",
          SimpleExpr(Name("$simplified_unique_name_0", _, None), _, None),
          _, None);
      ] -> true
      | _ -> false
    )
;;

let var_assign_from_tuple_test = gen_module_test "var_assign_from_tuple_test"
    "i, j = (-1,f(-1))"
    (function
      | [
        Assign(
          "$normalized_unique_name_0",
          Call(
            Name("f", _, None),
            [Literal(Num(Int(Neg), _, None), _, None)],
            _, None),
          _, None);

        Assign(
          "$normalized_unique_name_1",
          Tuple(
            [
              Literal(Num(Int(Neg), _, None), _, None);
              Name("$normalized_unique_name_0", _, None);
            ],
            _, None),
          _, None);

        Assign(
          "$simplified_unique_name_0",
          SimpleExpr(Name("$normalized_unique_name_1", _, None),
                     _, None),
          _, None);

        Assign(
          "$normalized_unique_name_2",
          Attribute(
            Name("$simplified_unique_name_0", _, None),
            "__iter__",
            _, None),
          _, None);

        Assign(
          "$normalized_unique_name_3",
          Call(
            Name("$normalized_unique_name_2", _, None),
            [],
            _, None),
          _, None);

        Assign(
          "$normalized_unique_name_4",
          Attribute(
            Name("$normalized_unique_name_3", _, None),
            "next",
            _, None),
          _, None);

        Assign(
          "$simplified_unique_name_1",
          SimpleExpr(Name("$normalized_unique_name_4", _, None),
                     _, None),
          _, None);
        (* I literally just copy-pasted the output of the program here. So
           this one is more of a regression test. *)
        (Assign ("$normalized_unique_name_5",
                 (Call (
                     (Name ("$simplified_unique_name_1", _,
                            (Some _))),
                     [], _, (Some _))),
                 _, (Some _)));

        (Assign ("$simplified_unique_name_2",
                 (SimpleExpr (
                     (Name ("$normalized_unique_name_5", _,
                            (Some _))),
                     _, (Some _))),
                 _, (Some _)));

        (Assign ("$normalized_unique_name_6",
                 (Call (
                     (Name ("$simplified_unique_name_1", _,
                            (Some _))),
                     [], _, (Some _))),
                 _, (Some _)));

        (Assign ("$simplified_unique_name_3",
                 (SimpleExpr (
                     (Name ("$normalized_unique_name_6", _,
                            (Some _))),
                     _, (Some _))),
                 _, (Some _)));

        (Assign ("$normalized_unique_name_7",
                 (Call (
                     (Name ("$simplified_unique_name_1", _,
                            (Some _))),
                     [], _, (Some _))),
                 _, (Some _)));

        (SimpleExprStmt (
            (Name ("$normalized_unique_name_7", _,
                   (Some _))),
            _, (Some _)));

        (Assign ("$normalized_unique_name_8",
                 (Call (
                     (Name ("ValueError", _, (Some _))),
                     [Literal(Str (
                          (StringLiteral
                             "too many values to unpack"),
                          _, (Some _)), _, Some _)
                     ],
                     _, (Some _))),
                 _, (Some _)));

        (Raise (
            (Name ("$normalized_unique_name_8", _,
                   (Some _))),
            _, (Some _)));

        (Goto (_, _, (Some _)));

        (Catch ("$normalized_unique_name_9", _, (Some _)
               ));

        (Assign ("$normalized_unique_name_10",
                 (Call (
                     (Name ("type", _, (Some _))),
                     [(Name ("$normalized_unique_name_9", _,
                             (Some _)))
                     ],
                     _, (Some _))),
                 _, (Some _)));

        (If (
            Literal(Bool (true, _, (Some _)), _, Some _),
            [(Assign ("$normalized_unique_name_11",
                      (Compare (
                          (Name ("$normalized_unique_name_10", _,
                                 (Some _))),
                          Eq,
                          (Name ("StopIteration", _, (Some _))),
                          _, (Some _))),
                      _, (Some _)))
            ],
            [(Assign ("$normalized_unique_name_11",
                      (SimpleExpr (
                          Literal(Bool (false, _, (Some _)), _, Some _), _,
                          (Some _))),
                      _, (Some _)))
            ],
            _, (Some _)));

        (Assign ("$normalized_unique_name_12",
                 (BoolOp (
                     (Literal(Bool (true, _, (Some _)), _, Some _)),
                     And,
                     (Name ("$normalized_unique_name_11", _,
                            (Some _))),
                     _, (Some _))),
                 _, (Some _)));

        (If (
            (Name ("$normalized_unique_name_12", _,
                   (Some _))),
            [(Pass (_, (Some _)))],
            [(Raise (
                 (Name ("$normalized_unique_name_9", _,
                        (Some _))),
                 _, (Some _)))
            ],
            _, (Some _)));

        (Pass (_, (Some _)));

        (Goto (_, _, None));

        (Catch ("$normalized_unique_name_13", _, None));

        (Assign ("$normalized_unique_name_14",
                 (Call (
                     (Name ("type", _, None)),
                     [(Name ("$normalized_unique_name_13", _,
                             None))
                     ],
                     _, None)),
                 _, None));

        (If (
            (Literal(Bool (true, _, None), _, None)),
            [(Assign ("$normalized_unique_name_15",
                      (Compare (
                          (Name ("$normalized_unique_name_14", _,
                                 None)),
                          Eq,
                          (Name ("StopIteration", _, None)), _,
                          None)),
                      _, None))
            ],
            [(Assign ("$normalized_unique_name_15",
                      (SimpleExpr (
                          (Literal(Bool (false, _, None), _, None), _, None))),
                      _, None))
            ],
            _, None));

        (Assign ("$normalized_unique_name_16",
                 (BoolOp (
                     (Literal(Bool (true, _, None), _, None)),
                     And,
                     (Name ("$normalized_unique_name_15", _,
                            None)),
                     _, None)),
                 _, None));

        (If (
            (Name ("$normalized_unique_name_16", _, None)),
            [(Assign ("$normalized_unique_name_17",
                      (Call (
                          (Name ("ValueError", _, None)),
                          [Literal(Str (
                               StringAbstract, _, None), _, None)
                          ],
                          _, None)),
                      _, None));

             (Raise (
                 (Name ("$normalized_unique_name_17", _,
                        None)),
                 _, None))
            ],
            [(Raise (
                 (Name ("$normalized_unique_name_13", _,
                        None)),
                 _, None))
            ],
            _, None));

        (Pass (_, None));

        Assign(
          "$simplified_unique_name_4",
          SimpleExpr(Name("$simplified_unique_name_2", _, None),
                     _, None),
          _, None);

        Assign(
          "i",
          SimpleExpr(Name("$simplified_unique_name_4", _, None),
                     _, None),
          _, None);

        Assign(
          "$simplified_unique_name_5",
          SimpleExpr(Name("$simplified_unique_name_3", _, None),
                     _, None),
          _, None);

        Assign(
          "j",
          SimpleExpr(Name("$simplified_unique_name_5", _, None),
                     _, None),
          _, None);
      ] -> true
      | _ -> false
    )
;;

let assign_to_index_test = gen_module_test "assign_to_index_test"
    "list[1+2] = 3"
    (function
      | [
        Assign(
          "$simplified_unique_name_0",
          SimpleExpr(Literal(Num(Int(Pos), _, None), _, None), _, None),
          _, None);

        Assign(
          "$normalized_unique_name_0",
          Attribute(
            Name("list", _, None),
            "__setitem__",
            _, None),
          _, None);

        Assign(
          "$normalized_unique_name_1",
          BinOp(
            Literal(Num(Int(Pos), _, None), _, None),
            Add,
            Literal(Num(Int(Pos), _, None), _, None),
            _, None),
          _, None);

        Assign(
          "$normalized_unique_name_2",
          Call(
            Name("$normalized_unique_name_0", _, None),
            [
              Name("$normalized_unique_name_1", _, None);
              Name("$simplified_unique_name_0", _, None);
            ],
            _, None),
          _, None);

        SimpleExprStmt(
          Name("$normalized_unique_name_2", _, None),
          _, None);
      ] -> true
      | _ -> false
    )
;;

let assign_to_slice_test = gen_module_test "assign_to_slice_test"
    "list[1:2] = 3"
    (function
      | [
        Assign(
          "$simplified_unique_name_0",
          SimpleExpr(Literal(Num(Int(Pos), _, None), _, None), _, None),
          _, None);

        Assign(
          "$normalized_unique_name_0",
          Attribute(
            Name("list", _, None),
            "__setitem__",
            _, None),
          _, None);

        Assign(
          "$normalized_unique_name_1",
          Call(
            Name("slice", _, None),
            [
              Literal(Num(Int(Pos), _, None), _, None);
              Literal(Num(Int(Pos), _, None), _, None);
              Name("None", _, None);
            ],
            _, None),
          _, None);

        Assign(
          "$normalized_unique_name_2",
          Call(
            Name("$normalized_unique_name_0", _, None),
            [
              Name("$normalized_unique_name_1", _, None);
              Name("$simplified_unique_name_0", _, None);
            ],
            _, None),
          _, None);

        SimpleExprStmt(
          Name("$normalized_unique_name_2", _, None),
          _, None);
      ] -> true
      | _ -> false
    )
;;

let assign_to_attribute_test = gen_module_test "assign_to_attribute_test"
    "obj.member = 7"
    (function
      | [
        Assign(
          "$simplified_unique_name_0",
          SimpleExpr(Literal(Num(Int(Pos), _, None), _, None), _, None),
          _, None);

        Assign(
          "$normalized_unique_name_0",
          Attribute(
            Name("obj", _, None),
            "__setattr__",
            _, None),
          _, None);

        Assign(
          "$normalized_unique_name_1",
          Call(
            Name("$normalized_unique_name_0", _, None),
            [
              Literal(Str(StringLiteral("member"), _, None), _, None);
              Name("$simplified_unique_name_0", _, None);
            ],
            _, None),
          _, None);

        SimpleExprStmt(Name("$normalized_unique_name_1", _, None),
                       _, None);

      ] -> true
      | _ -> false
    )
;;

let var_aug_assign_test = gen_module_test "var_aug_assign_test"
    "x *= -5"
    (function
      | [
        Assign(
          "$normalized_unique_name_0",
          BinOp(Name("x", _, None),
                Mult,
                Literal(Num(Int(Neg), _, None), _, None),
                _, None),
          _, None);

        Assign(
          "$simplified_unique_name_0",
          SimpleExpr(Name("$normalized_unique_name_0", _, None), _, None),
          _, None);

        Assign(
          "x",
          SimpleExpr(Name("$simplified_unique_name_0", _, None), _, None),
          _, None)
      ] -> true
      | _ -> false
    )
;;

let var_cmp_test = gen_module_test "var_cmp_test"
    "x <= 9000+1 > True"
    (function
      | [
        If(Literal(Bool(true, _, None), _, None),
           [
             Assign(
               "$normalized_unique_name_0",
               BinOp(
                 Literal(Num(Int(Pos), _, None), _, None),
                 Add,
                 Literal(Num(Int(Pos), _, None), _, None),
                 _, None),
               _, None);

             Assign(
               "$normalized_unique_name_1",
               Compare(
                 Name("x", _, None),
                 LtE,
                 Name("$normalized_unique_name_0", _, None),
                 _, None),
               _, None)
           ],
           [
             Assign(
               "$normalized_unique_name_1",
               SimpleExpr(Literal(Bool(false, _, None), _, None), _, None),
               _, None)
           ],
           _, None);

        Assign(
          "$normalized_unique_name_2",
          BoolOp(
            Literal(Bool(true, _, None), _, None),
            And,
            Name("$normalized_unique_name_1", _, None),
            _, None),
          _, None);

        If(
          Name("$normalized_unique_name_2", _, None),
          [
            Assign(
              "$normalized_unique_name_3",
              Compare(
                Name("$normalized_unique_name_0", _, None),
                Gt,
                Literal(Bool(true, _, None), _, None),
                _, None),
              _, None)
          ],
          [
            Assign(
              "$normalized_unique_name_3",
              SimpleExpr(Literal(Bool(false, _, None), _, None), _, None),
              _, None)
          ],
          _, None);

        Assign(
          "$normalized_unique_name_4",
          BoolOp(
            Name("$normalized_unique_name_2", _, None),
            And,
            Name("$normalized_unique_name_3", _, None),
            _, None),
          _, None);

        SimpleExprStmt(
          Name("$normalized_unique_name_4", _, None),
          _, None);
      ] -> true
      | _ -> false
    )
;;

let funcdef_test = gen_module_test "funcdef_test"
    "def test_function(arg1,arg2):\n\treturn arg1"
    (function
      | [
        FunctionDef("test_function",
                    [
                      "arg1";
                      "arg2";
                    ],
                    [ (* Body *)
                      Return(Some(Name("arg1", _, None)),
                             _, None)
                    ],
                    _, None)
      ] -> true
      | _ -> false
    )
;;

let call_test = gen_module_test "call_test"
    "func(1,x-1,get_arg('foo'))"
    (function
      | [
        Assign(
          "$normalized_unique_name_0",
          BinOp(
            Name("x", _, None),
            Sub,
            Literal(Num(Int(Pos), _, None), _, None),
            _, None),
          _, None);

        Assign(
          "$normalized_unique_name_1",
          Call(
            Name("get_arg", _, None),
            [Literal(Str(StringLiteral("foo"), _, None), _, None)],
            _, None),
          _, None);

        Assign(
          "$normalized_unique_name_2",
          Call(
            Name("func", _, None),
            [
              Literal(Num(Int(Pos), _, None), _, None);
              Name("$normalized_unique_name_0", _, None);
              Name("$normalized_unique_name_1", _, None);
            ],
            _, None),
          _, None);

        SimpleExprStmt(
          Name("$normalized_unique_name_2", _, None),
          _, None);
      ] -> true
      | _ -> false
    )
;;

let attribute_test = gen_module_test "attribute_test"
    "obj.member_var"
    (function
      | [
        Assign(
          "$normalized_unique_name_0",
          Attribute(
            Name("obj", _, None),
            "member_var",
            _, None),
          _, None);

        SimpleExprStmt(
          Name("$normalized_unique_name_0", _, None),
          _, None);
      ] -> true
      | _ -> false
    )
;;

let attribute_call_test = gen_module_test "attribute_call_test"
    "obj.member_func()"
    (function
      | [
        Assign(
          "$normalized_unique_name_0",
          Attribute(
            Name("obj", _, None),
            "member_func",
            _, None),
          _, None);

        Assign(
          "$normalized_unique_name_1",
          Call(
            Name("$normalized_unique_name_0", _, None),
            [],
            _, None),
          _, None);

        SimpleExprStmt(
          Name("$normalized_unique_name_1", _, None),
          _, None);
      ] -> true
      | _ -> false
    )
;;

let if_test = gen_module_test "if_test"
    "if x > 1+1:\n\tx = foo()\nelif x < 0.0: error('x < 0')\nelse: pass"
    (function
      | [
        If(
          Literal(Bool(true, _, None), _, None),
          [
            Assign(
              "$normalized_unique_name_0",
              BinOp(
                Literal(Num(Int(Pos), _, None), _, None),
                Add,
                Literal(Num(Int(Pos), _, None), _, None),
                _, None),
              _, None);

            Assign(
              "$normalized_unique_name_1",
              Compare(
                Name("x", _, None),
                Gt,
                Name("$normalized_unique_name_0", _, None),
                _, None),
              _, None);
          ],
          [
            Assign(
              "$normalized_unique_name_1",
              SimpleExpr(Literal(Bool(false, _, None), _, None), _, None),
              _, None);
          ],
          _, None);

        Assign(
          "$normalized_unique_name_2",
          BoolOp(
            Literal(Bool(true, _, None), _, None),
            And,
            Name("$normalized_unique_name_1", _, None),
            _, None),
          _, None);

        If(
          Name("$normalized_unique_name_2", _, None),
          [
            Assign(
              "$normalized_unique_name_3",
              Call(
                Name("foo", _, None),
                [],
                _, None),
              _, None);

            Assign(
              "$simplified_unique_name_0",
              SimpleExpr(
                Name("$normalized_unique_name_3", _, None),
                _, None),
              _, None);

            Assign(
              "x",
              SimpleExpr(
                Name("$simplified_unique_name_0", _, None),
                _, None),
              _, None);
          ],
          [
            If( (* Computing x < 0.0 *)
              Literal(Bool(true, _, None), _, None),
              [
                Assign(
                  "$normalized_unique_name_4",
                  Compare(
                    Name("x", _, None),
                    Lt,
                    Literal(Num(Float(Zero), _, None), _, None),
                    _, None),
                  _, None);
              ],
              [
                Assign(
                  "$normalized_unique_name_4",
                  SimpleExpr(Literal(Bool(false, _, None), _, None), _, None),
                  _, None);
              ],
              _, None);

            Assign(
              "$normalized_unique_name_5",
              BoolOp(
                Literal(Bool(true, _, None), _, None),
                And,
                Name("$normalized_unique_name_4", _, None),
                _, None),
              _, None);

            If( (* Elif stmt *)
              Name("$normalized_unique_name_5", _, None),
              [
                Assign(
                  "$normalized_unique_name_6",
                  Call(
                    Name("error", _, None),
                    [Literal(Str(StringLiteral("x < 0"), _, None), _, None)],
                    _, None),
                  _, None);

                SimpleExprStmt(
                  Name("$normalized_unique_name_6", _, None),
                  _, None);
              ],
              [
                Pass(_, None);
              ],
              _, None)
          ],
          _, None)
      ] -> true
      | _ -> false
    )
;;

let print_test = gen_module_test "print_test"
    "print 1,x(1+1),'foo'"
    (function
      | [
        Assign(
          "$normalized_unique_name_0",
          BinOp(
            Literal(Num(Int(Pos), _, None), _, None),
            Add,
            Literal(Num(Int(Pos), _, None), _, None),
            _, None),
          _, None);

        Assign(
          "$normalized_unique_name_1",
          Call(
            Name("x", _, None),
            [Name("$normalized_unique_name_0", _, None)],
            _, None),
          _, None);

        Print(None,
              [
                Literal(Num(Int(Pos), _, None), _, None);
                Name("$normalized_unique_name_1", _, None);
                Literal(Str(StringLiteral("foo"), _, None), _, None);
              ],
              true,
              _, None)
      ] -> true
      | _ -> false
    )
;;

let tuple_test = gen_module_test "tuple_test"
    "(1,1+1,(2,f(2)),'foo')"
    (function
      | [
        Assign( (* 1+1 *)
          "$normalized_unique_name_0",
          BinOp(
            Literal(Num(Int(Pos), _, None), _, None),
            Add,
            Literal(Num(Int(Pos), _, None), _, None),
            _, None),
          _, None);

        Assign( (* f(2) *)
          "$normalized_unique_name_1",
          Call(
            Name("f", _, None),
            [Literal(Num(Int(Pos), _, None), _, None)],
            _, None),
          _, None);

        Assign( (* (2, f(2)) *)
          "$normalized_unique_name_2",
          Tuple(
            [
              Literal(Num(Int(Pos), _, None), _, None);
              Name("$normalized_unique_name_1", _, None);
            ],
            _, None),
          _, None);

        Assign(
          "$normalized_unique_name_3",
          Tuple(
            [
              Literal(Num(Int(Pos), _, None), _, None);
              Name("$normalized_unique_name_0", _, None);
              Name("$normalized_unique_name_2", _, None);
              Literal(Str(StringLiteral("foo"), _, None), _, None);
            ],
            _, None),
          _, None);

        SimpleExprStmt(
          Name("$normalized_unique_name_3", _, None),
          _, None);
      ] -> true
      | _ -> false
    )
;;

let while_test = gen_module_test "while_test"
    "while x < 9001:\n\tx = x+1"
    (function
      | [
        Pass(loop_start, None);

        If(
          Literal(Bool(true, _, None), _, None),
          [
            Assign(
              "$normalized_unique_name_0",
              Compare(
                Name("x", _, None),
                Lt,
                Literal(Num(Int(Pos), _, None), _, None),
                _, None),
              _, None);
          ],
          [
            Assign(
              "$normalized_unique_name_0",
              SimpleExpr(Literal(Bool(false, _, None), _, None), _, None),
              _, None);
          ],
          _, None);

        Assign(
          "$normalized_unique_name_1",
          BoolOp(
            Literal(Bool(true, _, None), _, None),
            And,
            Name("$normalized_unique_name_0", _, None),
            _, None),
          _, None);

        Assign(
          "$normalized_unique_name_2",
          UnaryOp(
            Not,
            Name("$normalized_unique_name_1", _, None),
            _, None),
          _, None);

        If(
          Name("$normalized_unique_name_2", _, None),
          [
            Goto(loop_end_jump, _, None);
          ],
          [],
          _, None);

        Assign(
          "$normalized_unique_name_3",
          BinOp(Name("x", _, None),
                Add,
                Literal(Num(Int(Pos), _, None), _, None),
                _, None),
          _, None);

        Assign(
          "$simplified_unique_name_0",
          SimpleExpr(
            Name("$normalized_unique_name_3", _, None),
            _, None),
          _, None);

        Assign(
          "x",
          SimpleExpr(
            Name("$simplified_unique_name_0", _, None),
            _, None),
          _, None);

        Goto(loop_start_jump, _, None);

        Pass(loop_end, None);
      ] -> (loop_start_jump == loop_start) &&
           (loop_end_jump == loop_end)
      | _ -> false
    )
;;

let break_test = gen_module_test "break_test"
    "while x < 9001:\n\tbreak"
    (function
      | [
        Pass(loop_start, None);

        If(
          Literal(Bool(true, _, None), _, None),
          [
            Assign(
              "$normalized_unique_name_0",
              Compare(
                Name("x", _, None),
                Lt,
                Literal(Num(Int(Pos), _, None), _, None),
                _, None),
              _, None);
          ],
          [
            Assign(
              "$normalized_unique_name_0",
              SimpleExpr(Literal(Bool(false, _, None), _, None), _, None),
              _, None);
          ],
          _, None);

        Assign(
          "$normalized_unique_name_1",
          BoolOp(
            Literal(Bool(true, _, None), _, None),
            And,
            Name("$normalized_unique_name_0", _, None),
            _, None),
          _, None);

        Assign(
          "$normalized_unique_name_2",
          UnaryOp(
            Not,
            Name("$normalized_unique_name_1", _, None),
            _, None),
          _, None);

        If(
          Name("$normalized_unique_name_2", _, None),
          [
            Goto(loop_end_jump, _, None);
          ],
          [],
          _, None);

        Goto(break_jump, _, None);

        Goto(loop_start_jump, _, None);

        Pass(loop_end, None);

      ] -> (loop_start_jump == loop_start) &&
           (break_jump == loop_end) &&
           (loop_end_jump == loop_end)
      | _ -> false
    )
;;

let continue_test = gen_module_test "continue_test"
    "while x < 9001:\n\tcontinue"
    (function
      | [
        Pass(loop_start, None);

        If(
          Literal(Bool(true, _, None), _, None),
          [
            Assign(
              "$normalized_unique_name_0",
              Compare(
                Name("x", _, None),
                Lt,
                Literal(Num(Int(Pos), _, None), _, None),
                _, None),
              _, None);
          ],
          [
            Assign(
              "$normalized_unique_name_0",
              SimpleExpr(Literal(Bool(false, _, None), _, None), _, None),
              _, None);
          ],
          _, None);

        Assign(
          "$normalized_unique_name_1",
          BoolOp(
            Literal(Bool(true, _, None), _, None),
            And,
            Name("$normalized_unique_name_0", _, None),
            _, None),
          _, None);

        Assign(
          "$normalized_unique_name_2",
          UnaryOp(
            Not,
            Name("$normalized_unique_name_1", _, None),
            _, None),
          _, None);

        If(
          Name("$normalized_unique_name_2", _, None),
          [
            Goto(loop_end_jump, _, None);
          ],
          [],
          _, None);

        Goto(continue_jump, _, None);

        Goto(loop_start_jump, _, None);

        Pass(loop_end, None);

      ] -> (loop_start_jump == loop_start) &&
           (continue_jump == loop_start) &&
           (loop_end_jump == loop_end)
      | _ -> false
    )
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
    (function
      | [Raise(
          Name("ValueError", _, None),
          _, None)] -> true
      | _ -> false
    )
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
    (function
      | [
        (* Try body *)
        Assign(
          "$simplified_unique_name_0",
          SimpleExpr(Literal(Num(Int(Pos), _, Some(catch_uid_check_1)),
                             _, Some(_)), _, Some(_)),
          _, Some(_));

        Assign(
          "x",
          SimpleExpr(Name("$simplified_unique_name_0",
                          _, Some(_)),
                     _, Some(catch_uid_check_2)),
          _, Some(_));

        Goto(handler_end_jump, _, None);

        (* Catch exception *)
        Catch("$normalized_unique_name_0", catch_uid, None);

        (* Handler 1 - construct test expression*)
        Assign(
          "$normalized_unique_name_1",
          Call(
            Name("type", _, None),
            [Name("$normalized_unique_name_0", _, None)],
            _, None),
          _, None);

        If(
          Literal(Bool(true, _, None), _, None),
          [
            Assign(
              "$normalized_unique_name_2",
              Compare(
                Name("$normalized_unique_name_1", _, None),
                Eq,
                Name("ValueError", _, None),
                _, None),
              _, None);
          ],
          [
            Assign(
              "$normalized_unique_name_2",
              SimpleExpr(Literal(Bool(false, _, None), _, None),
                         _, None),
              _, None);
          ],
          _, None);

        Assign(
          "$normalized_unique_name_3",
          BoolOp(
            Literal(Bool(true, _, None), _, None),
            And,
            Name("$normalized_unique_name_2", _, None),
            _, None),
          _, None);

        (* Handler body *)
        If(
          Name("$normalized_unique_name_3", _, None),
          [
            Assign(
              "$normalized_unique_name_4",
              Call(
                Name("get_error", _, None),
                [],
                _, None),
              _, None);

            Print(
              None,
              [
                Literal(Str(StringLiteral("Error:"), _, None), _, None);
                Name("$normalized_unique_name_4", _, None);
              ],
              true,
              _, None)
          ],
          [
            (* Construct handler 2 test *)
            Assign(
              "$normalized_unique_name_5",
              Call(
                Name("type", _, None),
                [Name("$normalized_unique_name_0", _, None)],
                _, None),
              _, None);

            If(
              Literal(Bool(true, _, None), _, None),
              [
                Assign(
                  "$normalized_unique_name_6",
                  Compare(
                    Name("$normalized_unique_name_5", _, None),
                    Eq,
                    Name("StopIteration", _, None),
                    _, None),
                  _, None);
              ],
              [
                Assign(
                  "$normalized_unique_name_6",
                  SimpleExpr(Literal(Bool(false, _, None), _, None),
                             _, None),
                  _, None);
              ],
              _, None);

            Assign(
              "$normalized_unique_name_7",
              BoolOp(
                Literal(Bool(true, _, None), _, None),
                And,
                Name("$normalized_unique_name_6", _, None),
                _, None),
              _, None);

            (* Handler 2 body *)
            If(
              Name("$normalized_unique_name_7", _, None),
              [
                Assign(
                  "e",
                  SimpleExpr(
                    Name("$normalized_unique_name_0", _, None),
                    _, None),
                  _, None);

                Print (None,
                       [Name("e",_, None)],
                       true,
                       _, None)
              ],
              [
                Raise(Name("$normalized_unique_name_0", _, None),
                      _, None);
              ],
              _, None);
          ],
          _, None);

        Pass(handler_end, None);
      ] -> (handler_end_jump == handler_end) &&
           (catch_uid_check_1 == catch_uid) &&
           (catch_uid_check_2 == catch_uid)
      | _ -> false
    )
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
let big_test = gen_module_test "big_test"
    (triangle_def ^ "\n[triangle(1),triangle(7)]")
    (function
      | [
        FunctionDef(
          "triangle",
          ["n"],
          [ (* Body *)
            Assign(
              "$simplified_unique_name_0",
              SimpleExpr(Literal(Num(Int(Zero), _, None), _, None), _, None),
              _, None);

            Assign(
              "count",
              SimpleExpr(Name("$simplified_unique_name_0", _, None),
                         _, None),
              _, None);

            Assign(
              "$simplified_unique_name_1",
              SimpleExpr(Literal(Num(Int(Zero), _, None), _, None), _, None),
              _, None);

            Assign(
              "i",
              SimpleExpr(Name("$simplified_unique_name_1", _, None),
                         _, None),
              _, None);

            Pass(loop_start, None);

            If(
              Literal(Bool(true, _, None), _, None),
              [
                Assign(
                  "$normalized_unique_name_0",
                  Compare(
                    Name("count", _, None),
                    Lt,
                    Name("n", _, None),
                    _, None),
                  _, None);
              ],
              [
                Assign(
                  "$normalized_unique_name_0",
                  SimpleExpr(Literal(Bool(false, _, None), _, None), _, None),
                  _, None);
              ],
              _, None);

            Assign(
              "$normalized_unique_name_1",
              BoolOp(
                Literal(Bool(true, _, None), _, None),
                And,
                Name("$normalized_unique_name_0", _, None),
                _, None),
              _, None);

            Assign(
              "$normalized_unique_name_2",
              UnaryOp(
                Not,
                Name("$normalized_unique_name_1", _, None),
                _, None),
              _, None);

            If(
              Name("$normalized_unique_name_2", _, None),
              [
                Goto(loop_end_jump, _, None);
              ],
              [],
              _, None);

            Assign(
              "$normalized_unique_name_3",
              BinOp(Name("i", _, None),
                    Add,
                    Name("count", _, None),
                    _, None),
              _, None);

            Assign(
              "$simplified_unique_name_2",
              SimpleExpr(Name("$normalized_unique_name_3", _, None),
                         _, None),
              _, None);

            Assign(
              "i",
              SimpleExpr(
                Name("$simplified_unique_name_2", _, None),
                _, None),
              _, None);

            Assign(
              "$normalized_unique_name_4",
              BinOp(Name("count", _, None),
                    Add,
                    Literal(Num(Int(Pos), _, None), _, None),
                    _, None),
              _, None);

            Assign(
              "$simplified_unique_name_3",
              SimpleExpr(Name("$normalized_unique_name_4", _, None),
                         _, None),
              _, None);

            Assign(
              "count",
              SimpleExpr(
                Name("$simplified_unique_name_3", _, None),
                _, None),
              _, None);

            Goto(loop_start_jump, _, None);

            Pass(loop_end, None);

            Return(Some(Name("i", _, None)), _, None);
          ],
          _, None);

        Assign(
          "$normalized_unique_name_5",
          Call(
            Name("triangle", _, None),
            [Literal(Num(Int(Pos), _, None), _, None)],
            _, None),
          _, None);

        Assign(
          "$normalized_unique_name_6",
          Call(
            Name("triangle", _, None),
            [Literal(Num(Int(Pos), _, None), _, None)],
            _, None),
          _, None);

        Assign(
          "$normalized_unique_name_7",
          List(
            [
              Name("$normalized_unique_name_5", _, None);
              Name("$normalized_unique_name_6", _, None);
            ],
            _, None),
          _, None);

        SimpleExprStmt(
          Name("$normalized_unique_name_7", _, None),
          _, None);
      ] -> (loop_start_jump == loop_start) &&
           (loop_end_jump == loop_end)
      | _ -> false
    )
;;

(* Tests of lists and slicing *)
let list_str = "[1,2,f(3),'four','five',2+4]";;

let list_test = gen_module_test "list_test"
    list_str
    (function
      | [
        Assign(
          "$normalized_unique_name_0",
          Call(
            Name("f", _, None),
            [Literal(Num(Int(Pos), _, None), _, None)],
            _, None),
          _, None);

        Assign(
          "$normalized_unique_name_1",
          BinOp(
            Literal(Num(Int(Pos), _, None), _, None),
            Add,
            Literal(Num(Int(Pos), _, None), _, None),
            _, None),
          _, None);

        Assign(
          "$normalized_unique_name_2",
          List(
            [
              Literal(Num(Int(Pos), _, None), _, None);
              Literal(Num(Int(Pos), _, None), _, None);
              Name("$normalized_unique_name_0", _, None);
              Literal(Str(StringLiteral("four"), _, None), _, None);
              Literal(Str(StringLiteral("five"), _, None), _, None);
              Name("$normalized_unique_name_1", _, None);
            ],
            _, None),
          _, None);

        SimpleExprStmt(Name("$normalized_unique_name_2", _, None),
                       _, None)
      ] -> true
      | _ -> false
    )
;;

let tests =
  "abstract_ast">:::
  [
    uid_test;
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
