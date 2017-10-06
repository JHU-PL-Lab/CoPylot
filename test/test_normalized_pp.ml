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
open Python2_normalization_ctx

(* TODO: Currently this has all the same tests as test_normalized_ast,
   and so is redundant. It should be changed to more properly unit test just
   the pretty-printer, which means it should take a module to print directly.
   Of course, this is a pain to type out manually so I haven't done it. *)
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
      let pyssembly = pp_to_string pp_modl actual in
      assert_equal ~printer:(fun x -> x) ~cmp:String.equal expected pyssembly
  )
;;

let literal_tests =
  [
    gen_module_test "int+_test" "4" "   3:    :F:  Int+;";
    gen_module_test "int0_test" "0" "   3:    :F:  Int0;";
    gen_module_test "int-_test" "-4" "   3:    :F:  Int-;";

    gen_module_test "float+_test" "4.0" "   3:    :F:  Float+;";
    gen_module_test "float0_test" "0.0" "   3:    :F:  Float0;";
    gen_module_test "float-_test" "-4.0" "   3:    :F:  Float-;";

    gen_module_test "string_test" "'foo'" "   3:    :F:  \"foo\";";

    gen_module_test "bool_true_test" "True" "   3:    :F:  true;";
    gen_module_test "bool_false_test" "False" "   3:    :F:  false;";
  ]
;;

let stmt_tests =
  [
    gen_module_test "return_test" "return x" "   2:    :F:  return x;";

    gen_module_test "print_test1" "print x" "   3:    :F:  print x;";
    gen_module_test "print_test2" "print x,y" "   5:    :F:  print x, y;";

    gen_module_test "raise_test" "raise x" "   3:    :F:  raise x;";

    gen_module_test "pass_test" "pass" "   1:    :F:  pass;";
  ]
;;

let attribute_test = gen_module_test "attribute_test"
    "x.mem"
    ("   4:    :F:  $norm0 = x.mem;" ^
     "\n   7:    :F:  $norm0;")
;;

let call_test = gen_module_test "call_test"
    "foo()"
    ("   4:    :F:  $norm0 = foo();" ^
     "\n   7:    :F:  $norm0;")
;;

let call_args_test = gen_module_test "call_test"
    "foo(1,x,2+3)"
    ("   7:    :F:  $norm0 = Int+.__add__;" ^
     "\n  13:    :F:  $norm1 = $norm0(Int+);" ^
     "\n  20:    :F:  $norm2 = foo(Int+, x, $norm1);" ^
     "\n  23:    :F:  $norm2;")

;;

let assign_test = gen_module_test "assign_test"
    "x = 5"
    ("   4:    :F:  $simp0 = Int+;" ^
     "\n   8:    :F:  x = $simp0;")
;;

let augassign_test = gen_module_test "augassign_test"
    "x += 5"
    ("   4:    :F:  $norm0 = x.__add__;" ^
     "\n  10:    :F:  $norm1 = $norm0(Int+);" ^
     "\n  14:    :F:  $simp0 = $norm1;" ^
     "\n  18:    :F:  x = $simp0;")
;;

let multiassign_test = gen_module_test "multiassign_test"
    "x = y = 5"
    ("   4:    :F:  $simp0 = Int+;" ^
     "\n   8:    :F:  x = $simp0;" ^
     "\n  12:    :F:  y = $simp0;")
;;

let assign_to_member_test = gen_module_test "assign_to_member_test"
    "x.mem = 5"
    ("   4:    :F:  $simp0 = Int+;" ^
     "\n   8:    :F:  $norm0 = x.__setattr__;" ^
     "\n  16:    :F:  $norm1 = $norm0(\"mem\", $simp0);" ^
     "\n  19:    :F:  $norm1;")
;;

let assign_to_index_test = gen_module_test "assign_to_index_test"
    "list[5] = 5"
    ("   4:    :F:  $simp0 = Int+;" ^
     "\n   8:    :F:  $norm0 = list.__setitem__;" ^
     "\n  16:    :F:  $norm1 = $norm0(Int+, $simp0);" ^
     "\n  19:    :F:  $norm1;")
;;

let assign_to_slice_test = gen_module_test "assign_to_slice_test"
    "list[1:2] = 5"
    ("   4:    :F:  $simp0 = Int+;" ^
     "\n   8:    :F:  $norm0 = list.__setitem__;" ^
     "\n  19:    :F:  $norm1 = slice(Int+, Int+, None);" ^
     "\n  26:    :F:  $norm2 = $norm0($norm1, $simp0);" ^
     "\n  29:    :F:  $norm2;")
;;

let assign_from_tuple_test = gen_module_test "assign_from_tuple_test"
    "x,y = (1,2)"
    ("   6:    :F:  $norm0 = (Int+, Int+);" ^
     "\n  10:    :F:  $simp0 = $norm0;" ^
     "\n  14:    :F:  $norm1 = $simp0.__iter__;" ^
     "\n  18:    :F:  $norm2 = $norm1();" ^
     "\n  22:    :F:  $norm3 = $norm2.next;" ^
     "\n  26:    :F:  $simp1 = $norm3;" ^ (* $simp1 is iter.next *)
     "\n  32:  27:F:  $norm5 = $simp1();" ^
     "\n  36:  27:F:  $simp2 = $norm5;" ^
     "\n  40:  27:F:  $norm6 = $simp1();" ^
     "\n  44:  27:F:  $simp3 = $norm6;" ^
     "\n  50:  45:F:  $norm8 = $simp1();" ^ (* Should raise StopIteration *)
     "\n  53:  45:F:  $norm8;" ^
     "\n  59:  45:F:  $norm9 = ValueError(\"too many values to unpack\");" ^
     "\n  62:  45:F:  raise $norm9;" ^
     "\n  90:  27:F:  goto 46;" ^
     "\n  45:  27:F:  catch $norm7;" ^
     "\n  68:  27:F:  $norm10 = type($norm7);" ^
     "\n  72:  27:F:  $norm11 = $norm10.__eq__;" ^
     "\n  75:  27:F:  $norm12 = $norm11(StopIteration);" ^
     "\n  84:  27:F:  $norm13 = bool($norm12);" ^
     "\n  89:  27:F:  goto 87 if not $norm13;" ^ (* If wrong exception type *)
     "\n  77:  27:F:  pass;" ^
     "\n  86:  27:F:  goto 85;" ^
     "\n  87:  27:F:  pass;" ^
     "\n  80:  27:F:  raise $norm7;" ^
     "\n  85:  27:F:  pass;" ^
     "\n  46:  27:F:  pass;" ^
     "\n 126:    :F:  goto 28;" ^
     "\n  27:    :F:  catch $norm4;" ^
     "\n  96:    :F:  $norm14 = type($norm4);" ^
     "\n 100:    :F:  $norm15 = $norm14.__eq__;" ^
     "\n 103:    :F:  $norm16 = $norm15(StopIteration);" ^
     "\n 120:    :F:  $norm18 = bool($norm16);" ^
     "\n 125:    :F:  goto 123 if not $norm18;" ^
     "\n 110:    :F:  $norm17 = ValueError(StringAbstract);" ^
     "\n 113:    :F:  raise $norm17;" ^
     "\n 122:    :F:  goto 121;" ^
     "\n 123:    :F:  pass;" ^
     "\n 116:    :F:  raise $norm4;" ^
     "\n 121:    :F:  pass;" ^
     "\n  28:    :F:  pass;" ^
     "\n 130:    :F:  $simp4 = $simp2;" ^
     "\n 134:    :F:  x = $simp4;" ^
     "\n 138:    :F:  $simp5 = $simp3;" ^
     "\n 142:    :F:  y = $simp5;")
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
    ("   4:    :F:  $norm0 = x.__pos__;" ^
     "\n   8:    :F:  $norm1 = $norm0();" ^
     "\n  11:    :F:  $norm1;")
;;

let unop_minus_test = gen_module_test "unop_minus_test"
    "-x"
    ("   4:    :F:  $norm0 = x.__neg__;" ^
     "\n   8:    :F:  $norm1 = $norm0();" ^
     "\n  11:    :F:  $norm1;")
;;

let unop_not_test = gen_module_test "unop_not_test"
    "not x"
    ("   5:    :F:  $norm1 = bool(x);" ^
     "\n  18:    :F:  goto 16 if not $norm1;" ^
     "\n   9:    :F:  $norm0 = false;" ^
     "\n  15:    :F:  goto 14;" ^
     "\n  16:    :F:  pass;" ^
     "\n  13:    :F:  $norm0 = true;" ^
     "\n  14:    :F:  pass;" ^
     "\n  21:    :F:  $norm0;")
;;

let gen_binop_test (name : string) (opstring : string) (opfunc : string ) =
  gen_module_test name ("x" ^ opstring ^ "y")
    begin
      "   4:    :F:  $norm0 = x." ^ opfunc ^ ";" ^
      "\n  10:    :F:  $norm1 = $norm0(y);" ^
      "\n  13:    :F:  $norm1;"
    end
;;

let boolop_and_test = gen_module_test "boolop_and_test"
    "x and 5 and True"
    ("   4:    :F:  $norm0 = x;" ^
     "\n   9:    :F:  $norm2 = bool($norm0);" ^
     "\n  44:    :F:  goto 42 if not $norm2;" ^
     "\n  13:    :F:  $norm3 = Int+;" ^
     "\n  18:    :F:  $norm5 = bool($norm3);" ^
     "\n  31:    :F:  goto 29 if not $norm5;" ^
     "\n  22:    :F:  $norm4 = true;" ^
     "\n  28:    :F:  goto 27;" ^
     "\n  29:    :F:  pass;" ^
     "\n  26:    :F:  $norm4 = $norm3;" ^
     "\n  27:    :F:  pass;" ^
     "\n  35:    :F:  $norm1 = $norm4;" ^
     "\n  41:    :F:  goto 40;" ^
     "\n  42:    :F:  pass;" ^
     "\n  39:    :F:  $norm1 = $norm0;" ^
     "\n  40:    :F:  pass;" ^
     "\n  47:    :F:  $norm1;")
;;

let boolop_or_test = gen_module_test "boolop_or_test"
    "x or 5 or True"
    ("   4:    :F:  $norm0 = x;" ^
     "\n   9:    :F:  $norm2 = bool($norm0);" ^
     "\n  44:    :F:  goto 42 if not $norm2;" ^
     "\n  13:    :F:  $norm1 = $norm0;" ^
     "\n  41:    :F:  goto 40;" ^
     "\n  42:    :F:  pass;" ^
     "\n  17:    :F:  $norm3 = Int+;" ^
     "\n  22:    :F:  $norm5 = bool($norm3);" ^
     "\n  35:    :F:  goto 33 if not $norm5;" ^
     "\n  26:    :F:  $norm4 = $norm3;" ^
     "\n  32:    :F:  goto 31;" ^
     "\n  33:    :F:  pass;" ^
     "\n  30:    :F:  $norm4 = true;" ^
     "\n  31:    :F:  pass;" ^
     "\n  39:    :F:  $norm1 = $norm4;" ^
     "\n  40:    :F:  pass;" ^
     "\n  47:    :F:  $norm1;")
;;

let gen_cmpop_test (name : string) (opstring : string) (opfunc : string ) =
  gen_module_test name ("x " ^ opstring ^ " y")
    begin
      "   4:    :F:  $norm0 = x." ^ opfunc ^ ";" ^
      "\n   7:    :F:  $norm1 = $norm0(y);" ^
      "\n  10:    :F:  $norm1;"
    end
;;

let multi_compare_test = gen_module_test "multi_compare_test"
    "x < y < z"
    (  "   4:    :F:  $norm0 = x.__lt__;" ^
     "\n   7:    :F:  $norm1 = $norm0(y);" ^
     "\n  12:    :F:  $norm3 = bool($norm1);" ^
     "\n  32:    :F:  goto 30 if not $norm3;" ^
     "\n  16:    :F:  $norm4 = y.__lt__;" ^
     "\n  19:    :F:  $norm5 = $norm4(z);" ^
     "\n  23:    :F:  $norm2 = $norm5;" ^
     "\n  29:    :F:  goto 28;" ^
     "\n  30:    :F:  pass;" ^
     "\n  27:    :F:  $norm2 = $norm1;" ^
     "\n  28:    :F:  pass;" ^
     "\n  35:    :F:  $norm2;")
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
    ("   7:    :F:  $norm0 = Int+.__sub__;" ^
     "\n  13:    :F:  $norm1 = $norm0(Int+);" ^
     "\n  20:    :F:  $norm2 = [Int+, Float0, \"foo\", $norm1];" ^
     "\n  23:    :F:  $norm2;")

;;

let tuple_test = gen_module_test "tuple_test"
    "(1,0.0,'foo',f(3+33))"
    ("   8:    :F:  $norm0 = Int+.__add__;" ^
     "\n  14:    :F:  $norm1 = $norm0(Int+);" ^
     "\n  19:    :F:  $norm2 = f($norm1);" ^
     "\n  26:    :F:  $norm3 = (Int+, Float0, \"foo\", $norm2);" ^
     "\n  29:    :F:  $norm3;")

;;

let index_test = gen_module_test "index_test"
    "list[0]"
    ("   4:    :F:  $norm0 = list.__getitem__;" ^
     "\n  10:    :F:  $norm1 = $norm0(Int0);" ^
     "\n  13:    :F:  $norm1;")
;;

let slice_test = gen_module_test "slice_test"
    "list[1:2]"
    ("   4:    :F:  $norm0 = list.__getitem__;" ^
     "\n  15:    :F:  $norm1 = slice(Int+, Int+, None);" ^
     "\n  20:    :F:  $norm2 = $norm0($norm1);" ^
     "\n  23:    :F:  $norm2;")
;;

let if_test = gen_module_test "if_test"
    "if x: z = 1\nelif y: z = -1\nelse: 0"
    begin
      "  34:    :F:  $norm1 = bool(x);" ^
      "\n  39:    :F:  goto 37 if not $norm1;" ^
      "\n   5:    :F:  $simp1 = Int+;" ^
      "\n   9:    :F:  z = $simp1;" ^
      "\n  36:    :F:  goto 35;" ^
      "\n  37:    :F:  pass;" ^
      "\n  25:    :F:  $norm0 = bool(y);" ^
      "\n  30:    :F:  goto 28 if not $norm0;" ^
      "\n  14:    :F:  $simp0 = Int-;" ^
      "\n  18:    :F:  z = $simp0;" ^
      "\n  27:    :F:  goto 26;" ^
      "\n  28:    :F:  pass;" ^
      "\n  21:    :F:  Int0;" ^
      "\n  26:    :F:  pass;" ^
      "\n  35:    :F:  pass;"
    end
;;

let ifexp_test = gen_module_test "ifexp_test"
    "z = 1 if x else -1 if y else 0"
    begin
      "   5:    :F:  $norm1 = bool(x);" ^
      "\n  36:    :F:  goto 34 if not $norm1;" ^
      "\n   9:    :F:  $norm0 = Int+;" ^
      "\n  33:    :F:  goto 32;" ^
      "\n  34:    :F:  pass;" ^
      "\n  14:    :F:  $norm3 = bool(y);" ^
      "\n  27:    :F:  goto 25 if not $norm3;" ^
      "\n  18:    :F:  $norm2 = Int-;" ^
      "\n  24:    :F:  goto 23;" ^
      "\n  25:    :F:  pass;" ^
      "\n  22:    :F:  $norm2 = Int0;" ^
      "\n  23:    :F:  pass;" ^
      "\n  31:    :F:  $norm0 = $norm2;" ^
      "\n  32:    :F:  pass;" ^
      "\n  40:    :F:  $simp0 = $norm0;" ^
      "\n  44:    :F:  z = $simp0;"
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
      "   6:   1:F:  $simp0 = Int+;" ^
      "\n  10:   1:F:  x = $simp0;" ^
      "\n  14:   1:F:  $norm1 = somefunction();" ^
      "\n  17:   1:F:  $norm1;" ^
      "\n  75:    :F:  goto 2;" ^
      "\n   1:    :F:  catch $norm0;" ^
      "\n  23:    :F:  $norm2 = type($norm0);" ^
      "\n  27:    :F:  $norm3 = $norm2.__eq__;" ^
      "\n  30:    :F:  $norm4 = $norm3(ValueError);" ^
      "\n  69:    :F:  $norm9 = bool($norm4);" ^
      "\n  74:    :F:  goto 72 if not $norm9;" ^
      "\n  35:    :F:  e = $norm0;" ^
      "\n  36:    :F:  pass;" ^
      "\n  71:    :F:  goto 70;" ^
      "\n  72:    :F:  pass;" ^
      "\n  42:    :F:  $norm5 = type($norm0);" ^
      "\n  46:    :F:  $norm6 = $norm5.__eq__;" ^
      "\n  49:    :F:  $norm7 = $norm6(Int);" ^
      "\n  60:    :F:  $norm8 = bool($norm7);" ^
      "\n  65:    :F:  goto 63 if not $norm8;" ^
      "\n  53:    :F:  print \"got an int\";" ^
      "\n  62:    :F:  goto 61;" ^
      "\n  63:    :F:  pass;" ^
      "\n  56:    :F:  raise $norm0;" ^
      "\n  61:    :F:  pass;" ^
      "\n  70:    :F:  pass;" ^
      "\n   2:    :F:  pass;"
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
      "  11:    :F:  def f() {" ^
      "\n   4:    :F:    $simp0 = f;" ^
      "\n   8:    :F:    f$1_x = $simp0;" ^
      "\n  10:    :F:    return n;" ^
      "\n};" ^
      "\n  15:    :F:  $norm0 = f();" ^
      "\n  19:    :F:  $simp1 = $norm0;" ^
      "\n  23:    :F:  x = $simp1;"
    end
;;

let while_test = gen_module_test "while_test"
    "while x < 3:\n\tx += 1"
    ("  13:    :T:  pass;" ^
     "\n   4:    :T:  $norm0 = x.__lt__;" ^
     "\n   7:    :T:  $norm1 = $norm0(Int+);" ^
     "\n  12:    :T:  $norm2 = bool($norm1);" ^
     "\n  34:    :T:  goto 14 if not $norm2;" ^
     "\n  18:    :T:  $norm3 = x.__add__;" ^
     "\n  24:    :T:  $norm4 = $norm3(Int+);" ^
     "\n  28:    :T:  $simp0 = $norm4;" ^
     "\n  32:    :T:  x = $simp0;" ^
     "\n  35:    :T:  goto 13;" ^
     "\n  14:    :T:  pass;")

;;

let break_test = gen_module_test "break_test"
    "while x < 3:\n\tbreak"
    ("  13:    :T:  pass;" ^
     "\n   4:    :T:  $norm0 = x.__lt__;" ^
     "\n   7:    :T:  $norm1 = $norm0(Int+);" ^
     "\n  12:    :T:  $norm2 = bool($norm1);" ^
     "\n  17:    :T:  goto 14 if not $norm2;" ^
     "\n  15:    :T:  goto 14;" ^
     "\n  18:    :T:  goto 13;" ^
     "\n  14:    :T:  pass;")
;;

let continue_test = gen_module_test "continue_test"
    "while x < 3:\n\tcontinue"
    ("  13:    :T:  pass;" ^
     "\n   4:    :T:  $norm0 = x.__lt__;" ^
     "\n   7:    :T:  $norm1 = $norm0(Int+);" ^
     "\n  12:    :T:  $norm2 = bool($norm1);" ^
     "\n  17:    :T:  goto 14 if not $norm2;" ^
     "\n  15:    :T:  goto 13;" ^
     "\n  18:    :T:  goto 13;" ^
     "\n  14:    :T:  pass;")
;;

let for_test = gen_module_test "for_test"
    begin
      "for i in [-1,1]:" ^
      "\n  if i < 3:" ^
      "\n    print i"
    end
    begin
      "   6:    :F:  $norm0 = [Int-, Int+];" ^
      "\n  10:    :F:  $norm1 = $norm0.__iter__;" ^
      "\n  14:    :F:  $norm2 = $norm1();" ^
      "\n  18:    :F:  $norm3 = $norm2.next;" ^
      "\n  22:    :F:  $simp1 = $norm3;" ^
      "\n  26:    :F:  $simp0 = $simp1;" ^
      "\n  34:  27:T:  pass;" ^
      "\n  33:  27:T:  $norm5 = bool(true);" ^
      "\n  69:  27:T:  goto 35 if not $norm5;" ^
      "\n  39:  27:T:  $norm6 = $simp0();" ^
      "\n  43:  27:T:  $simp2 = $norm6;" ^
      "\n  47:  27:T:  i = $simp2;" ^
      "\n  51:  27:T:  $norm7 = i.__lt__;" ^
      "\n  54:  27:T:  $norm8 = $norm7(Int+);" ^
      "\n  62:  27:T:  $norm9 = bool($norm8);" ^
      "\n  67:  27:T:  goto 65 if not $norm9;" ^
      "\n  58:  27:T:  print i;" ^
      "\n  64:  27:T:  goto 63;" ^
      "\n  65:  27:T:  pass;" ^
      "\n  63:  27:T:  pass;" ^
      "\n  70:  27:T:  goto 34;" ^
      "\n  35:  27:T:  pass;" ^
      "\n  98:    :F:  goto 28;" ^
      "\n  27:    :F:  catch $norm4;" ^
      "\n  76:    :F:  $norm10 = type($norm4);" ^
      "\n  80:    :F:  $norm11 = $norm10.__eq__;" ^
      "\n  83:    :F:  $norm12 = $norm11(StopIteration);" ^
      "\n  92:    :F:  $norm13 = bool($norm12);" ^
      "\n  97:    :F:  goto 95 if not $norm13;" ^
      "\n  85:    :F:  pass;" ^
      "\n  94:    :F:  goto 93;" ^
      "\n  95:    :F:  pass;" ^
      "\n  88:    :F:  raise $norm4;" ^
      "\n  93:    :F:  pass;" ^
      "\n  28:    :F:  pass;"
    end
;;

let tests =
  "test_normalized_pp">:::
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
    while_test;
    break_test;
    continue_test;
    for_test;
  ]
