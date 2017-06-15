open OUnit2
open Batteries
open Jhupllib
open Pp_utils
open Python2_parser
open Lexing
module Rename = Python2_rename_ast
module Simplified = Python2_simplified_ast
module Lift = Python2_ast_lifter
module Simplify = Python2_ast_simplifier
module Normalize = Python2_ast_normalizer
open Python2_normalized_ast
open Python2_normalized_ast_pretty
open Uid_generation

let string_of_modl m = Pp_utils.pp_to_string pp_modl m;;

let parse_from_string_safe str =
  try
    parse_from_string str
  with
  | Python2_parser.Parse_error p ->
    assert_failure (Printf.sprintf "Error in line %d, col %d."
                      p.pos_lnum p.pos_cnum)
;;

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

and collect_uids_stmt s =
  let rest =
    match s.body with
    | Assign (_, {uid = u; body = Literal(FunctionVal(_, body)); _})
      -> [u] @ List.concat (List.map collect_uids_stmt body)
    | Assign (_, {uid = u; _})
      -> [u]
    | _ -> []
  in
  s.uid::rest
;;

let id_map = Rename.Id_map.empty;;

let gen_module_test (name : string) (prog : string)
    (expected : string)=
  name>::
  ( fun _ ->
      let concrete = parse_from_string_safe (prog ^ "\n") in
      let abstract = Lift.lift_modl concrete in
      let renamed = Rename.rename_modl id_map [] abstract in
      let simplified =
        (* Occasionally a test will fail; resetting here might help *)
        Simplify.reset_unique_name ();
        Simplify.toggle_short_names true;
        Simplify.simplify_modl renamed in
      Simplify.reset_unique_name ();
      let ctx = create_new_uid_context () in
      let actual =
        Normalize.toggle_short_names true;
        Normalize.normalize_modl ctx simplified in
      Normalize.reset_unique_name ();
      let distinct_uids = verify_unique_uids actual in
      assert_bool ("Repeated UIDs:\n" ^ string_of_modl actual)
        distinct_uids;
      let pyssembly = pp_to_string pp_modl actual in
      assert_equal ~printer:(fun x -> x) ~cmp:String.equal expected pyssembly
  )
;;

let gen_literal_test name prog exp =
  gen_module_test name prog
    ("@   2:    :F:  $norm0 = " ^ exp ^ ";" ^
     "\n@   3:    :F:  $norm0;")
;;

let literal_tests =
  [
    gen_literal_test "int+_test" "4" "Int+";
    gen_literal_test "int0_test" "0" "Int0";
    gen_literal_test "int-_test" "-4" "Int-";

    gen_literal_test "float+_test" "4.0" "Float+";
    gen_literal_test "float0_test" "0.0" "Float0";
    gen_literal_test "float-_test" "-4.0" "Float-";

    gen_literal_test "string_test" "'foo'" "\"foo\"";

    gen_literal_test "bool_true_test" "True" "true";
    gen_literal_test "bool_false_test" "False" "false";
  ]
;;

let stmt_tests =
  [
    gen_module_test "return_test" "return x" "@   1:    :F:  return x;";

    gen_module_test "print_test1" "print x" "@   1:    :F:  print x;";
    gen_module_test "print_test2" "print x,y" "@   1:    :F:  print x, y;";

    gen_module_test "raise_test" "raise x" "@   1:    :F:  raise x;";

    gen_module_test "pass_test" "pass" "@   1:    :F:  pass;";
  ]
;;

let attribute_test = gen_module_test "attribute_test"
    "x.mem"
    ("@   2:    :F:  $norm0 = x.mem;" ^
     "\n@   3:    :F:  $norm0;")
;;

let call_test = gen_module_test "call_test"
    "foo()"
    ("@   2:    :F:  $norm0 = foo();" ^
     "\n@   3:    :F:  $norm0;")
;;

let call_args_test = gen_module_test "call_args_test"
    "foo(1,x,2+3)"
    ("@   2:    :F:  $norm0 = Int+;" ^
     "\n@   4:    :F:  $norm1 = Int+;" ^
     "\n@   6:    :F:  $norm2 = $norm1.__add__;" ^
     "\n@   8:    :F:  $norm3 = Int+;" ^
     "\n@  10:    :F:  $norm4 = $norm2($norm3);" ^
     "\n@  12:    :F:  $norm5 = foo($norm0, x, $norm4);" ^
     "\n@  13:    :F:  $norm5;")

;;

let assign_test = gen_module_test "assign_test"
    "x = 5"
    ("@   2:    :F:  $norm0 = Int+;" ^
     "\n@   4:    :F:  $simp0 = $norm0;" ^
     "\n@   6:    :F:  x = $simp0;")
;;

let augassign_test = gen_module_test "augassign_test"
    "x += 5"
    ("@   2:    :F:  $norm0 = x.__add__;" ^
     "\n@   4:    :F:  $norm1 = Int+;" ^
     "\n@   6:    :F:  $norm2 = $norm0($norm1);" ^
     "\n@   8:    :F:  $simp0 = $norm2;" ^
     "\n@  10:    :F:  x = $simp0;")
;;

let multiassign_test = gen_module_test "multiassign_test"
    "x = y = 5"
    ("@   2:    :F:  $norm0 = Int+;" ^
     "\n@   4:    :F:  $simp0 = $norm0;" ^
     "\n@   6:    :F:  x = $simp0;" ^
     "\n@   8:    :F:  y = $simp0;")
;;

let assign_to_member_test = gen_module_test "assign_to_member_test"
    "x.mem = 5"
    ("@   2:    :F:  $norm0 = Int+;" ^
     "\n@   4:    :F:  $simp0 = $norm0;" ^
     "\n@   6:    :F:  $norm1 = x.__setattr__;" ^
     "\n@   8:    :F:  $norm2 = \"mem\";" ^
     "\n@  10:    :F:  $norm3 = $norm1($norm2, $simp0);" ^
     "\n@  11:    :F:  $norm3;")
;;

let assign_to_index_test = gen_module_test "assign_to_index_test"
    "list[5] = 5"
    ("@   2:    :F:  $norm0 = Int+;" ^
     "\n@   4:    :F:  $simp0 = $norm0;" ^
     "\n@   6:    :F:  $norm1 = list.__setitem__;" ^
     "\n@   8:    :F:  $norm2 = Int+;" ^
     "\n@  10:    :F:  $norm3 = $norm1($norm2, $simp0);" ^
     "\n@  11:    :F:  $norm3;")
;;

let assign_to_slice_test = gen_module_test "assign_to_slice_test"
    "list[1:2] = 5"
    ("@   2:    :F:  $norm0 = Int+;" ^
     "\n@   4:    :F:  $simp0 = $norm0;" ^
     "\n@   6:    :F:  $norm1 = list.__setitem__;" ^
     "\n@   8:    :F:  $norm2 = slice;" ^
     "\n@  10:    :F:  $norm3 = Int+;" ^
     "\n@  12:    :F:  $norm4 = Int+;" ^
     "\n@  14:    :F:  $norm5 = $norm2($norm3, $norm4, None);" ^
     "\n@  16:    :F:  $norm6 = $norm1($norm5, $simp0);" ^
     "\n@  17:    :F:  $norm6;")
;;

let assign_from_tuple_test = gen_module_test "assign_from_tuple_test"
    "x,y = (1,2)"
    ("@   2:    :F:  $norm0 = Int+;" ^
     "\n@   4:    :F:  $norm1 = Int+;" ^
     "\n@   6:    :F:  $norm2 = ($norm0, $norm1);" ^
     "\n@   8:    :F:  $simp0 = $norm2;" ^
     "\n@  10:    :F:  $norm3 = $simp0.__iter__;" ^
     "\n@  12:    :F:  $norm4 = $norm3();" ^
     "\n@  14:    :F:  $norm5 = $norm4.next;" ^
     "\n@  16:    :F:  $simp1 = $norm5;" ^
     "\n@  20:  17:F:  $norm7 = $simp1();" ^
     "\n@  22:  17:F:  $simp2 = $norm7;" ^
     "\n@  24:  17:F:  $norm8 = $simp1();" ^
     "\n@  26:  17:F:  $simp3 = $norm8;" ^
     "\n@  30:  27:F:  $norm10 = $simp1();" ^
     "\n@  31:  27:F:  $norm10;" ^
     "\n@  33:  27:F:  $norm11 = \"too many values to unpack\";" ^
     "\n@  35:  27:F:  $norm12 = ValueError($norm11);" ^
     "\n@  36:  27:F:  raise $norm12;" ^
     "\n@  55:  17:F:  goto 28;" ^
     "\n@  27:  17:F:  catch $norm9;" ^
     "\n@  38:  17:F:  $norm13 = type;" ^
     "\n@  40:  17:F:  $norm14 = $norm13($norm9);" ^
     "\n@  42:  17:F:  $norm15 = $norm14.__eq__;" ^
     "\n@  44:  17:F:  $norm16 = $norm15(StopIteration);" ^
     "\n@  46:  17:F:  $norm17 = bool;" ^
     "\n@  48:  17:F:  $norm18 = $norm17($norm16);" ^
     "\n@  54:  17:F:  goto 53 if not $norm18;" ^
     "\n@  49:  17:F:  pass;" ^
     "\n@  52:  17:F:  goto 51;" ^
     "\n@  53:  17:F:  pass;" ^
     "\n@  50:  17:F:  raise $norm9;" ^
     "\n@  51:  17:F:  pass;" ^
     "\n@  28:  17:F:  pass;" ^
     "\n@  78:    :F:  goto 18;" ^
     "\n@  17:    :F:  catch $norm6;" ^
     "\n@  57:    :F:  $norm19 = type;" ^
     "\n@  59:    :F:  $norm20 = $norm19($norm6);" ^
     "\n@  61:    :F:  $norm21 = $norm20.__eq__;" ^
     "\n@  63:    :F:  $norm22 = $norm21(StopIteration);" ^
     "\n@  65:    :F:  $norm23 = bool;" ^
     "\n@  67:    :F:  $norm24 = $norm23($norm22);" ^
     "\n@  77:    :F:  goto 76 if not $norm24;" ^
     "\n@  69:    :F:  $norm25 = StringAbstract;" ^
     "\n@  71:    :F:  $norm26 = ValueError($norm25);" ^
     "\n@  72:    :F:  raise $norm26;" ^
     "\n@  75:    :F:  goto 74;" ^
     "\n@  76:    :F:  pass;" ^
     "\n@  73:    :F:  raise $norm6;" ^
     "\n@  74:    :F:  pass;" ^
     "\n@  18:    :F:  pass;" ^
     "\n@  80:    :F:  $simp4 = $simp2;" ^
     "\n@  82:    :F:  x = $simp4;" ^
     "\n@  84:    :F:  $simp5 = $simp3;" ^
     "\n@  86:    :F:  y = $simp5;")
;;

let assignment_tests =
  [
    assign_test;
    augassign_test;
    multiassign_test;
    assign_to_member_test;
    assign_to_index_test;
    assign_to_slice_test;
    assign_from_tuple_test;
  ]
;;

let unop_plus_test = gen_module_test "unop_plus_test"
    "+x"
    ("@   2:    :F:  $norm0 = x.__pos__;" ^
     "\n@   4:    :F:  $norm1 = $norm0();" ^
     "\n@   5:    :F:  $norm1;")
;;

let unop_minus_test = gen_module_test "unop_minus_test"
    "-x"
    ("@   2:    :F:  $norm0 = x.__neg__;" ^
     "\n@   4:    :F:  $norm1 = $norm0();" ^
     "\n@   5:    :F:  $norm1;")
;;

let unop_not_test = gen_module_test "unop_not_test"
    "not x"
    ("@   2:    :F:  $norm1 = bool;" ^
     "\n@   4:    :F:  $norm2 = $norm1(x);" ^
     "\n@  16:    :F:  goto 15 if not $norm2;" ^
     "\n@   6:    :F:  $norm3 = false;" ^
     "\n@   8:    :F:  $norm0 = $norm3;" ^
     "\n@  14:    :F:  goto 13;" ^
     "\n@  15:    :F:  pass;" ^
     "\n@  10:    :F:  $norm4 = true;" ^
     "\n@  12:    :F:  $norm0 = $norm4;" ^
     "\n@  13:    :F:  pass;" ^
     "\n@  17:    :F:  $norm0;")
;;

let gen_binop_test (name : string) (opstring : string) (opfunc : string ) =
  gen_module_test name ("x" ^ opstring ^ "y")
    begin
      "@   2:    :F:  $norm0 = x." ^ opfunc ^ ";" ^
      "\n@   4:    :F:  $norm1 = $norm0(y);" ^
      "\n@   5:    :F:  $norm1;"
    end
;;

let boolop_and_test = gen_module_test "boolop_and_test"
    "x and 5 and True"
    ("@   2:    :F:  $norm1 = bool;" ^
     "\n@   4:    :F:  $norm2 = $norm1(x);" ^
     "\n@  28:    :F:  goto 27 if not $norm2;" ^
     "\n@   6:    :F:  $norm3 = Int+;" ^
     "\n@   8:    :F:  $norm5 = bool;" ^
     "\n@  10:    :F:  $norm6 = $norm5($norm3);" ^
     "\n@  20:    :F:  goto 19 if not $norm6;" ^
     "\n@  12:    :F:  $norm7 = true;" ^
     "\n@  14:    :F:  $norm4 = $norm7;" ^
     "\n@  18:    :F:  goto 17;" ^
     "\n@  19:    :F:  pass;" ^
     "\n@  16:    :F:  $norm4 = $norm3;" ^
     "\n@  17:    :F:  pass;" ^
     "\n@  22:    :F:  $norm0 = $norm4;" ^
     "\n@  26:    :F:  goto 25;" ^
     "\n@  27:    :F:  pass;" ^
     "\n@  24:    :F:  $norm0 = x;" ^
     "\n@  25:    :F:  pass;" ^
     "\n@  29:    :F:  $norm0;")
;;

let boolop_or_test = gen_module_test "boolop_or_test"
    "x or 5 or True"
    ("@   2:    :F:  $norm1 = bool;" ^
     "\n@   4:    :F:  $norm2 = $norm1(x);" ^
     "\n@  28:    :F:  goto 27 if not $norm2;" ^
     "\n@   6:    :F:  $norm0 = x;" ^
     "\n@  26:    :F:  goto 25;" ^
     "\n@  27:    :F:  pass;" ^
     "\n@   8:    :F:  $norm3 = Int+;" ^
     "\n@  10:    :F:  $norm5 = bool;" ^
     "\n@  12:    :F:  $norm6 = $norm5($norm3);" ^
     "\n@  22:    :F:  goto 21 if not $norm6;" ^
     "\n@  14:    :F:  $norm4 = $norm3;" ^
     "\n@  20:    :F:  goto 19;" ^
     "\n@  21:    :F:  pass;" ^
     "\n@  16:    :F:  $norm7 = true;" ^
     "\n@  18:    :F:  $norm4 = $norm7;" ^
     "\n@  19:    :F:  pass;" ^
     "\n@  24:    :F:  $norm0 = $norm4;" ^
     "\n@  25:    :F:  pass;" ^
     "\n@  29:    :F:  $norm0;")
;;

let gen_cmpop_test (name : string) (opstring : string) (opfunc : string ) =
  gen_module_test name ("x " ^ opstring ^ " y")
    begin
      "@   2:    :F:  $norm0 = x." ^ opfunc ^ ";" ^
      "\n@   4:    :F:  $norm1 = $norm0(y);" ^
      "\n@   5:    :F:  $norm1;"
    end
;;

let multi_compare_test = gen_module_test "multi_compare_test"
    "x < y < z"
    ("@   2:    :F:  $norm0 = x.__lt__;" ^
     "\n@   4:    :F:  $norm1 = $norm0(y);" ^
     "\n@   6:    :F:  $norm3 = bool;" ^
     "\n@   8:    :F:  $norm4 = $norm3($norm1);" ^
     "\n@  20:    :F:  goto 19 if not $norm4;" ^
     "\n@  10:    :F:  $norm5 = y.__lt__;" ^
     "\n@  12:    :F:  $norm6 = $norm5(z);" ^
     "\n@  14:    :F:  $norm2 = $norm6;" ^
     "\n@  18:    :F:  goto 17;" ^
     "\n@  19:    :F:  pass;" ^
     "\n@  16:    :F:  $norm2 = $norm1;" ^
     "\n@  17:    :F:  pass;" ^
     "\n@  21:    :F:  $norm2;")
;;

let operator_tests =
  [
    unop_plus_test;
    unop_minus_test;
    unop_not_test;
    gen_binop_test "add_test" "+" "__add__";
    gen_binop_test "sub_test" "-" "__sub__";
    gen_binop_test "mul_test" "*" "__mul__";
    gen_binop_test "div_test" "/" "__div__";
    gen_binop_test "mod_test" "%" "__mod__";
    gen_binop_test "pow_test" "**" "__pow__";
    boolop_and_test;
    boolop_or_test;
    gen_cmpop_test "eq_test" "==" "__eq__";
    gen_cmpop_test "ne_test" "!=" "__ne__";
    gen_cmpop_test "lt_test" "<"  "__lt__";
    gen_cmpop_test "le_test" "<=" "__le__";
    gen_cmpop_test "gt_test" ">"  "__gt__";
    gen_cmpop_test "ge_test" ">=" "__ge__";
    gen_cmpop_test "in_test" "in" "__contains__";
    multi_compare_test;
  ]
;;

let list_test = gen_module_test "list_test"
    "[1,0.0,'foo',1-9]"
    ("@   2:    :F:  $norm0 = Int+;" ^
     "\n@   4:    :F:  $norm1 = Float0;" ^
     "\n@   6:    :F:  $norm2 = \"foo\";" ^
     "\n@   8:    :F:  $norm3 = Int+;" ^
     "\n@  10:    :F:  $norm4 = $norm3.__sub__;" ^
     "\n@  12:    :F:  $norm5 = Int+;" ^
     "\n@  14:    :F:  $norm6 = $norm4($norm5);" ^
     "\n@  16:    :F:  $norm7 = [$norm0, $norm1, $norm2, $norm6];" ^
     "\n@  17:    :F:  $norm7;")

;;

let tuple_test = gen_module_test "tuple_test"
    "(1,0.0,'foo',f(3+33))"
    ("@   2:    :F:  $norm0 = Int+;" ^
     "\n@   4:    :F:  $norm1 = Float0;" ^
     "\n@   6:    :F:  $norm2 = \"foo\";" ^
     "\n@   8:    :F:  $norm3 = Int+;" ^
     "\n@  10:    :F:  $norm4 = $norm3.__add__;" ^
     "\n@  12:    :F:  $norm5 = Int+;" ^
     "\n@  14:    :F:  $norm6 = $norm4($norm5);" ^
     "\n@  16:    :F:  $norm7 = f($norm6);" ^
     "\n@  18:    :F:  $norm8 = ($norm0, $norm1, $norm2, $norm7);" ^
     "\n@  19:    :F:  $norm8;")

;;

let index_test = gen_module_test "index_test"
    "list[0]"
    ("@   2:    :F:  $norm0 = list.__getitem__;" ^
     "\n@   4:    :F:  $norm1 = Int0;" ^
     "\n@   6:    :F:  $norm2 = $norm0($norm1);" ^
     "\n@   7:    :F:  $norm2;")
;;

let slice_test = gen_module_test "slice_test"
    "list[1:2]"
    ("@   2:    :F:  $norm0 = list.__getitem__;" ^
     "\n@   4:    :F:  $norm1 = slice;" ^
     "\n@   6:    :F:  $norm2 = Int+;" ^
     "\n@   8:    :F:  $norm3 = Int+;" ^
     "\n@  10:    :F:  $norm4 = $norm1($norm2, $norm3, None);" ^
     "\n@  12:    :F:  $norm5 = $norm0($norm4);" ^
     "\n@  13:    :F:  $norm5;")
;;

let if_test = gen_module_test "if_test"
    "if x: z = 1\nelif y: z = -1\nelse: 0"
    begin
      "@   2:    :F:  $norm0 = bool;" ^
      "\n@   4:    :F:  $norm1 = $norm0(x);" ^
      "\n@  31:    :F:  goto 30 if not $norm1;" ^
      "\n@   6:    :F:  $norm2 = Int+;" ^
      "\n@   8:    :F:  $simp1 = $norm2;" ^
      "\n@  10:    :F:  z = $simp1;" ^
      "\n@  29:    :F:  goto 28;" ^
      "\n@  30:    :F:  pass;" ^
      "\n@  12:    :F:  $norm3 = bool;" ^
      "\n@  14:    :F:  $norm4 = $norm3(y);" ^
      "\n@  27:    :F:  goto 26 if not $norm4;" ^
      "\n@  16:    :F:  $norm5 = Int-;" ^
      "\n@  18:    :F:  $simp0 = $norm5;" ^
      "\n@  20:    :F:  z = $simp0;" ^
      "\n@  25:    :F:  goto 24;" ^
      "\n@  26:    :F:  pass;" ^
      "\n@  22:    :F:  $norm6 = Int0;" ^
      "\n@  23:    :F:  $norm6;" ^
      "\n@  24:    :F:  pass;" ^
      "\n@  28:    :F:  pass;"
    end
;;

let ifexp_test = gen_module_test "ifexp_test"
    "z = 1 if x else -1 if y else 0"
    begin
      "@   2:    :F:  $norm1 = bool;" ^
      "\n@   4:    :F:  $norm2 = $norm1(x);" ^
      "\n@  30:    :F:  goto 29 if not $norm2;" ^
      "\n@   6:    :F:  $norm3 = Int+;" ^
      "\n@   8:    :F:  $norm0 = $norm3;" ^
      "\n@  28:    :F:  goto 27;" ^
      "\n@  29:    :F:  pass;" ^
      "\n@  10:    :F:  $norm5 = bool;" ^
      "\n@  12:    :F:  $norm6 = $norm5(y);" ^
      "\n@  24:    :F:  goto 23 if not $norm6;" ^
      "\n@  14:    :F:  $norm7 = Int-;" ^
      "\n@  16:    :F:  $norm4 = $norm7;" ^
      "\n@  22:    :F:  goto 21;" ^
      "\n@  23:    :F:  pass;" ^
      "\n@  18:    :F:  $norm8 = Int0;" ^
      "\n@  20:    :F:  $norm4 = $norm8;" ^
      "\n@  21:    :F:  pass;" ^
      "\n@  26:    :F:  $norm0 = $norm4;" ^
      "\n@  27:    :F:  pass;" ^
      "\n@  32:    :F:  $simp0 = $norm0;" ^
      "\n@  34:    :F:  z = $simp0;"
    end
;;

let tryexcept_test = gen_module_test "tryexcept_test"
    begin
      "try:" ^
      "\n  x = 5" ^
      "\n  somefunction()" ^
      "\nexcept ValueError as e:" ^
      "\n  pass" ^
      "\nexcept Int:" ^
      "\n  print 'got an int'"
    end
    begin
      "@   4:   1:F:  $norm1 = Int+;" ^
      "\n@   6:   1:F:  $simp0 = $norm1;" ^
      "\n@   8:   1:F:  x = $simp0;" ^
      "\n@  10:   1:F:  $norm2 = somefunction();" ^
      "\n@  11:   1:F:  $norm2;" ^
      "\n@  51:    :F:  goto 2;" ^
      "\n@   1:    :F:  catch $norm0;" ^
      "\n@  13:    :F:  $norm3 = type;" ^
      "\n@  15:    :F:  $norm4 = $norm3($norm0);" ^
      "\n@  17:    :F:  $norm5 = $norm4.__eq__;" ^
      "\n@  19:    :F:  $norm6 = $norm5(ValueError);" ^
      "\n@  21:    :F:  $norm7 = bool;" ^
      "\n@  23:    :F:  $norm8 = $norm7($norm6);" ^
      "\n@  50:    :F:  goto 49 if not $norm8;" ^
      "\n@  25:    :F:  e = $norm0;" ^
      "\n@  26:    :F:  pass;" ^
      "\n@  48:    :F:  goto 47;" ^
      "\n@  49:    :F:  pass;" ^
      "\n@  28:    :F:  $norm9 = type;" ^
      "\n@  30:    :F:  $norm10 = $norm9($norm0);" ^
      "\n@  32:    :F:  $norm11 = $norm10.__eq__;" ^
      "\n@  34:    :F:  $norm12 = $norm11(Int);" ^
      "\n@  36:    :F:  $norm13 = bool;" ^
      "\n@  38:    :F:  $norm14 = $norm13($norm12);" ^
      "\n@  46:    :F:  goto 45 if not $norm14;" ^
      "\n@  40:    :F:  $norm15 = \"got an int\";" ^
      "\n@  41:    :F:  print $norm15;" ^
      "\n@  44:    :F:  goto 43;" ^
      "\n@  45:    :F:  pass;" ^
      "\n@  42:    :F:  raise $norm0;" ^
      "\n@  43:    :F:  pass;" ^
      "\n@  47:    :F:  pass;" ^
      "\n@   2:    :F:  pass;"
    end
;;

let funcdef_test = gen_module_test "funcdef_test"
    begin
      "def f():" ^
      "\n  x = f" ^
      "\n  return n" ^
      "\nx = f()"
    end
    begin
      "@   7:    :F:  f = fun() {" ^
      "\n@   2:    :F:    $simp0 = f;" ^
      "\n@   4:    :F:    f$1_x = $simp0;" ^
      "\n@   5:    :F:    return n;" ^
      "\n};" ^
      "\n@   9:    :F:  $norm0 = f();" ^
      "\n@  11:    :F:  $simp1 = $norm0;" ^
      "\n@  13:    :F:  x = $simp1;"
    end
;;

let funcdef_args_test = gen_module_test "funcdef_args_test"
    begin
      "def f(x,y):" ^
      "\n  x = f" ^
      "\n  return n" ^
      "\nx = f(x)"
    end
    begin
        "@   7:    :F:  f = fun(f$1_x, f$1_y) {" ^
      "\n@   2:    :F:    $simp0 = f;" ^
      "\n@   4:    :F:    f$1_x = $simp0;" ^
      "\n@   5:    :F:    return n;" ^
      "\n};" ^
      "\n@   9:    :F:  $norm0 = f(x);" ^
      "\n@  11:    :F:  $simp1 = $norm0;" ^
      "\n@  13:    :F:  x = $simp1;"
    end
;;

let while_test = gen_module_test "while_test"
    "while x < 3:\n\tx += 1"
    ("@  11:    :T:  pass;" ^
     "\n@   2:    :T:  $norm0 = Int+;" ^
     "\n@   4:    :T:  $norm1 = x.__lt__;" ^
     "\n@   6:    :T:  $norm2 = $norm1($norm0);" ^
     "\n@   8:    :T:  $norm3 = bool;" ^
     "\n@  10:    :T:  $norm4 = $norm3($norm2);" ^
     "\n@  23:    :T:  goto 12 if not $norm4;" ^
     "\n@  14:    :T:  $norm5 = x.__add__;" ^
     "\n@  16:    :T:  $norm6 = Int+;" ^
     "\n@  18:    :T:  $norm7 = $norm5($norm6);" ^
     "\n@  20:    :T:  $simp0 = $norm7;" ^
     "\n@  22:    :T:  x = $simp0;" ^
     "\n@  24:    :T:  goto 11;" ^
     "\n@  12:    :T:  pass;")

;;

let break_test = gen_module_test "break_test"
    "while x < 3:\n\tbreak"
    ("@  11:    :T:  pass;" ^
     "\n@   2:    :T:  $norm0 = Int+;" ^
     "\n@   4:    :T:  $norm1 = x.__lt__;" ^
     "\n@   6:    :T:  $norm2 = $norm1($norm0);" ^
     "\n@   8:    :T:  $norm3 = bool;" ^
     "\n@  10:    :T:  $norm4 = $norm3($norm2);" ^
     "\n@  14:    :T:  goto 12 if not $norm4;" ^
     "\n@  13:    :T:  goto 12;" ^
     "\n@  15:    :T:  goto 11;" ^
     "\n@  12:    :T:  pass;")
;;

let continue_test = gen_module_test "continue_test"
    "while x < 3:\n\tcontinue"
    ("@  11:    :T:  pass;" ^
     "\n@   2:    :T:  $norm0 = Int+;" ^
     "\n@   4:    :T:  $norm1 = x.__lt__;" ^
     "\n@   6:    :T:  $norm2 = $norm1($norm0);" ^
     "\n@   8:    :T:  $norm3 = bool;" ^
     "\n@  10:    :T:  $norm4 = $norm3($norm2);" ^
     "\n@  14:    :T:  goto 12 if not $norm4;" ^
     "\n@  13:    :T:  goto 11;" ^
     "\n@  15:    :T:  goto 11;" ^
     "\n@  12:    :T:  pass;")
;;

let for_test = gen_module_test "for_test"
    begin
      "for i in [-1,1]:" ^
      "\n  if i < 3:" ^
      "\n    print i"
    end
    begin
      "@   2:    :F:  $norm0 = Int-;" ^
      "\n@   4:    :F:  $norm1 = Int+;" ^
      "\n@   6:    :F:  $norm2 = [$norm0, $norm1];" ^
      "\n@   8:    :F:  $norm3 = $norm2.__iter__;" ^
      "\n@  10:    :F:  $norm4 = $norm3();" ^
      "\n@  12:    :F:  $norm5 = $norm4.next;" ^
      "\n@  14:    :F:  $simp1 = $norm5;" ^
      "\n@  16:    :F:  $simp0 = $simp1;" ^
      "\n@  25:  17:T:  pass;" ^
      "\n@  20:  17:T:  $norm7 = true;" ^
      "\n@  22:  17:T:  $norm8 = bool;" ^
      "\n@  24:  17:T:  $norm9 = $norm8($norm7);" ^
      "\n@  48:  17:T:  goto 26 if not $norm9;" ^
      "\n@  28:  17:T:  $norm10 = $simp0();" ^
      "\n@  30:  17:T:  $simp2 = $norm10;" ^
      "\n@  32:  17:T:  i = $simp2;" ^
      "\n@  34:  17:T:  $norm11 = Int+;" ^
      "\n@  36:  17:T:  $norm12 = i.__lt__;" ^
      "\n@  38:  17:T:  $norm13 = $norm12($norm11);" ^
      "\n@  40:  17:T:  $norm14 = bool;" ^
      "\n@  42:  17:T:  $norm15 = $norm14($norm13);" ^
      "\n@  47:  17:T:  goto 46 if not $norm15;" ^
      "\n@  43:  17:T:  print i;" ^
      "\n@  45:  17:T:  goto 44;" ^
      "\n@  46:  17:T:  pass;" ^
      "\n@  44:  17:T:  pass;" ^
      "\n@  49:  17:T:  goto 25;" ^
      "\n@  26:  17:T:  pass;" ^
      "\n@  68:    :F:  goto 18;" ^
      "\n@  17:    :F:  catch $norm6;" ^
      "\n@  51:    :F:  $norm16 = type;" ^
      "\n@  53:    :F:  $norm17 = $norm16($norm6);" ^
      "\n@  55:    :F:  $norm18 = $norm17.__eq__;" ^
      "\n@  57:    :F:  $norm19 = $norm18(StopIteration);" ^
      "\n@  59:    :F:  $norm20 = bool;" ^
      "\n@  61:    :F:  $norm21 = $norm20($norm19);" ^
      "\n@  67:    :F:  goto 66 if not $norm21;" ^
      "\n@  62:    :F:  pass;" ^
      "\n@  65:    :F:  goto 64;" ^
      "\n@  66:    :F:  pass;" ^
      "\n@  63:    :F:  raise $norm6;" ^
      "\n@  64:    :F:  pass;" ^
      "\n@  18:    :F:  pass;"
    end
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

let bad_break_test = expect_error_test "bad_break_test"
    "break"
    (Failure("'break' outside loop"))
;;

let bad_continue_test = expect_error_test "bad_continue_test"
    "continue"
    (Failure("'continue' not properly in loop"))

;;

let expected_failures =
  [
    bad_break_test;
    bad_continue_test;
  ]
;;

let tests =
  "normalized_pp">:::
  literal_tests @
  stmt_tests @
  operator_tests @
  assignment_tests @
  [
    attribute_test;
    call_test;
    call_args_test;
    list_test;
    tuple_test;
    index_test;
    slice_test;
    if_test;
    ifexp_test;
    tryexcept_test;
    funcdef_test;
    funcdef_args_test;
    while_test;
    break_test;
    continue_test;
    for_test;
  ] @
  expected_failures
