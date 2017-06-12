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

let int_test = gen_module_test "int_test"
    "4"
    "3::Int+"
;;

let funcdef =
  "def f():" ^
  "\n  x = f" ^
  "\n  return n" ^
  "\nx = f()"
;;

let funcdef_pp =
  "11::f() {" ^
  "\n    4::$simp0 = f" ^
  "\n    8::f$1_x = $simp0" ^
  "\n    10::return n" ^
  "\n}" ^
  "\n15::$norm0 = f()" ^
  "\n19::$simp1 = $norm0" ^
  "\n23::x = $simp1"
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
  "6::$norm0 = [Int-, Int+]" ^
  "\n10::$norm1 = $norm0.__iter__" ^
  "\n14::$norm2 = $norm1()" ^
  "\n18::$norm3 = $norm2.next" ^
  "\n22::$simp1 = $norm3" ^
  "\n26::$simp0 = $simp1" ^
  "\n34:27:pass" ^
  "\n33:27:$norm4 = bool(true)" ^
  "\n69:27:goto 35 if not $norm4" ^
  "\n39:27:$norm5 = $simp0()" ^
  "\n43:27:$simp2 = $norm5" ^
  "\n47:27:i = $simp2" ^
  "\n51:27:$norm6 = i.__lt__" ^
  "\n54:27:$norm7 = $norm6(Int+)" ^
  "\n62:27:$norm8 = bool($norm7)" ^
  "\n67:27:goto 65 if not $norm8" ^
  "\n58:27:print i" ^
  "\n64:27:goto 63" ^
  "\n65:27:pass" ^
  "\n63:27:pass" ^
  "\n70:27:goto 34" ^
  "\n35:27:pass" ^
  "\n99::goto 28" ^
  "\n27::pass" ^
  "\n71::catch $norm9" ^
  "\n77::$norm10 = type($norm9)" ^
  "\n81::$norm11 = $norm10.__eq__" ^
  "\n84::$norm12 = $norm11(StopIteration)" ^
  "\n93::$norm13 = bool($norm12)" ^
  "\n98::goto 96 if not $norm13" ^
  "\n86::pass" ^
  "\n95::goto 94" ^
  "\n96::pass" ^
  "\n89::raise $norm9" ^
  "\n94::pass" ^
  "\n28::pass"


let loop_test = gen_module_test "loop_test"
    for_loop
    for_loop_pp
;;

let tests =
  "normalized_pp">:::
  [
    int_test;
    funcdef_test;
    loop_test;
  ]
