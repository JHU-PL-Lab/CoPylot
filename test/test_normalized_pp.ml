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

let list_test = gen_module_test "list_test"
    "[1,0.0,'foo']"
    "   3:  0:F:  [Int+, Float0, \"foo\"];"
;;

let tuple_test = gen_module_test "list_test"
    "(1,0.0,'foo')"
    "   3:  0:F:  (Int+, Float0, \"foo\");"
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
  "\n  34:  27:F:  pass;" ^
  "\n  33:  27:F:  $norm4 = bool(true);" ^
  "\n  69:  27:F:  goto 35 if not $norm4;" ^
  "\n  39:  27:T:  $norm5 = $simp0();" ^
  "\n  43:  27:T:  $simp2 = $norm5;" ^
  "\n  47:  27:T:  i = $simp2;" ^
  "\n  51:  27:T:  $norm6 = i.__lt__;" ^
  "\n  54:  27:T:  $norm7 = $norm6(Int+);" ^
  "\n  62:  27:T:  $norm8 = bool($norm7);" ^
  "\n  67:  27:T:  goto 65 if not $norm8;" ^
  "\n  58:  27:F:  print i;" ^
  "\n  64:  27:T:  goto 63;" ^
  "\n  65:  27:T:  pass;" ^
  "\n  63:  27:T:  pass;" ^
  "\n  70:  27:F:  goto 34;" ^
  "\n  35:  27:F:  pass;" ^
  "\n  99:    :F:  goto 28;" ^
  "\n  27:    :F:  pass;" ^
  "\n  71:    :F:  catch $norm9;" ^
  "\n  77:    :F:  $norm10 = type($norm9);" ^
  "\n  81:    :F:  $norm11 = $norm10.__eq__;" ^
  "\n  84:    :F:  $norm12 = $norm11(StopIteration);" ^
  "\n  93:    :F:  $norm13 = bool($norm12);" ^
  "\n  98:    :F:  goto 96 if not $norm13;" ^
  "\n  86:    :F:  pass;" ^
  "\n  95:    :F:  goto 94;" ^
  "\n  96:    :F:  pass;" ^
  "\n  89:    :F:  raise $norm9;" ^
  "\n  94:    :F:  pass;" ^
  "\n  28:    :F:  pass;"

let loop_test = gen_module_test "loop_test"
    for_loop
    for_loop_pp
;;

let tests =
  "normalized_pp">:::
  literal_tests @
  [
    funcdef_test;
    loop_test;
  ]
