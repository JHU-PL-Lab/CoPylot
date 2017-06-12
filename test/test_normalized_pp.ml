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
  gen_module_test name ("x" ^ opstring ^ "1")
    begin
      "   4:    :F:  $norm0 = x." ^ opfunc ^ ";" ^
      "\n  10:    :F:  $norm1 = $norm0(Int+);" ^
      "\n  13:    :F:  $norm1;"
    end
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

  ]
;;

let list_test = gen_module_test "list_test"
    "[1,0.0,'foo']"
    ("   8:    :F:  $norm0 = [Int+, Float0, \"foo\"];" ^
     "\n  11:    :F:  $norm0;")
;;

let tuple_test = gen_module_test "tuple_test"
    "(1,0.0,'foo')"
    ("   8:    :F:  $norm0 = (Int+, Float0, \"foo\");" ^
     "\n  11:    :F:  $norm0;")
;;

let slice_test = gen_module_test "slice_test"
    "list[1:2]"
    ("   4:    :F:  $norm0 = list.__getitem__;" ^
     "\n  15:    :F:  $norm1 = slice(Int+, Int+, None);" ^
     "\n  20:    :F:  $norm2 = $norm0($norm1);" ^
     "\n  23:    :F:  $norm2;")
;;

let if_pp =
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
;;

let if_test = gen_module_test "if_test"
    "if x: z = 1\nelif y: z = -1\nelse: 0"
    if_pp
;;

let ifexp =
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
;;

let ifexp_test = gen_module_test "ifexp_test"
    "z = 1 if x else -1 if y else 0"
    ifexp
;;

let tryexcept =
  "try:" ^
  "\n  x = 5" ^
  "\n  somefunction()" ^
  "\nexcept ValueError as e:" ^
  "\n  pass" ^
  "\nexcept Int:" ^
  "\n  print 'got an int'"
;;

let tryexcept_pp =
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

;;

let tryexcept_test = gen_module_test "tryexcept_test"
    tryexcept
    tryexcept_pp
;;

let funcdef =
  "def f():" ^
  "\n  x = f" ^
  "\n  return n" ^
  "\nx = f()"
;;

let funcdef_pp =
  "  11:    :F:  def f() {" ^
  "\n   4:    :F:    $simp0 = f;" ^
  "\n   8:    :F:    f$1_x = $simp0;" ^
  "\n  10:    :F:    return n;" ^
  "\n};" ^
  "\n  15:    :F:  $norm0 = f();" ^
  "\n  19:    :F:  $simp1 = $norm0;" ^
  "\n  23:    :F:  x = $simp1;"
;;

let funcdef_test = gen_module_test "funcdef_test"
    funcdef
    funcdef_pp
;;

let for_loop =
  "for i in [-1,1]:" ^
  "\n  if i < 3:" ^
  "\n    print i"
;;

let for_loop_pp =
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
;;

let loop_test = gen_module_test "loop_test"
    for_loop
    for_loop_pp
;;

let tests =
  "normalized_pp">:::
  literal_tests @
  stmt_tests @
  operator_tests @
  [
    list_test;
    tuple_test;
    slice_test;
    if_test;
    ifexp_test;
    tryexcept_test;
    funcdef_test;
    loop_test;
  ]
