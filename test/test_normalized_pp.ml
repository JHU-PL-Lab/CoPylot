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
        Simplify.simplify_modl renamed in
      Simplify.reset_unique_name ();
      let ctx = create_new_uid_context () in
      let actual = Normalize.normalize_modl ctx simplified in
      Normalize.reset_unique_name ();
      let pyssembly = pp_to_string pp_modl actual in
      assert_equal ~printer:(fun x -> x) ~cmp:String.equal expected pyssembly
  )
;;

let int_test = gen_module_test "int_test"
    "4"
    "3::Pos"
;;

let funcdef =
  "def f():" ^
  "\n  x=f" ^
  "\n  return n" ^
  "\nx=f()"
;;

let funcdef_pp =
  "11::f(){" ^
  "\n    4::$simplified_unique_name_0=f" ^
  "\n    8::f$1_x=$simplified_unique_name_0" ^
  "\n    10::return(n)" ^
  "\n}" ^
  "\n15::$normalized_unique_name_0=f()" ^
  "\n19::$simplified_unique_name_1=$normalized_unique_name_0" ^
  "\n23::x=$simplified_unique_name_1"
;;

let funcdef_test = gen_module_test "funcdef_test"
    funcdef
    funcdef_pp
;;

let for_loop =
  "for i in [2,6]:" ^
  "\n  if i < 3:" ^
  "\n    print i"
;;

let for_loop_pp =
  "6::$normalized_unique_name_0=PosPos" ^
  "\n10::$normalized_unique_name_1=$normalized_unique_name_0.__iter__" ^
  "\n14::$normalized_unique_name_2=$normalized_unique_name_1()" ^
  "\n18::$normalized_unique_name_3=$normalized_unique_name_2.next" ^
  "\n22::$simplified_unique_name_1=$normalized_unique_name_3" ^
  "\n26::$simplified_unique_name_0=$simplified_unique_name_1" ^
  "\n34:27:pass" ^
  "\n33:27:$normalized_unique_name_4=bool()(true)" ^
  "\n69:27:goto 35 if not $normalized_unique_name_4" ^
  "\n39:27:$normalized_unique_name_5=$simplified_unique_name_0()" ^
  "\n43:27:$simplified_unique_name_2=$normalized_unique_name_5" ^
  "\n47:27:i=$simplified_unique_name_2" ^
  "\n51:27:$normalized_unique_name_6=i.__lt__" ^
  "\n54:27:$normalized_unique_name_7=$normalized_unique_name_6(Pos)" ^
  "\n62:27:$normalized_unique_name_8=bool()($normalized_unique_name_7)" ^
  "\n67:27:goto 65 if not $normalized_unique_name_8" ^
  "\n58:27:print(i) > " ^
  "\n64:27:goto 63" ^
  "\n65:27:pass" ^
  "\n63:27:pass" ^
  "\n70:27:goto 34" ^
  "\n35:27:pass" ^
  "\n99::goto 28" ^
  "\n27::pass" ^
  "\n71::catch($normalized_unique_name_9)" ^
  "\n77::$normalized_unique_name_10=type()($normalized_unique_name_9)" ^
  "\n81::$normalized_unique_name_11=$normalized_unique_name_10.__eq__" ^
  "\n84::$normalized_unique_name_12=$normalized_unique_name_11(StopIteration)" ^
  "\n93::$normalized_unique_name_13=bool()($normalized_unique_name_12)" ^
  "\n98::goto 96 if not $normalized_unique_name_13" ^
  "\n86::pass" ^
  "\n95::goto 94" ^
  "\n96::pass" ^
  "\n89::raise($normalized_unique_name_9)" ^
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
