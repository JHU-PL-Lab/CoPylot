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
  "12::f(){" ^
  "\n    2::$simplified_unique_name_0=f" ^
  "\n    6::f$1_x=$simplified_unique_name_0" ^
  "\n    10::return(n)" ^
  "\n}" ^
  "\n16::$normalized_unique_name_0=f()" ^
  "\n18::$simplified_unique_name_1=$normalized_unique_name_0" ^
  "\n22::x=$simplified_unique_name_1"
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
  "10::$normalized_unique_name_0=PosPos" ^
  "\n14::$normalized_unique_name_1=$normalized_unique_name_0.__iter__" ^
  "\n18::$normalized_unique_name_2=$normalized_unique_name_1()" ^
  "\n22::$normalized_unique_name_3=$normalized_unique_name_2.next" ^
  "\n24::$simplified_unique_name_1=$normalized_unique_name_3" ^
  "\n28::$simplified_unique_name_0=$simplified_unique_name_1" ^
  "\n42:31:pass" ^
  "\n40:31:$normalized_unique_name_4=bool()(true)" ^
  "\n79:31:goto 43 if not $normalized_unique_name_4" ^
  "\n47:31:$normalized_unique_name_5=$simplified_unique_name_0()" ^
  "\n49:31:$simplified_unique_name_2=$normalized_unique_name_5" ^
  "\n53:31:i=$simplified_unique_name_2" ^
  "\n60:31:$normalized_unique_name_6=i.__lt__" ^
  "\n63:31:$normalized_unique_name_7=$normalized_unique_name_6(Pos)" ^
  "\n72:31:$normalized_unique_name_8=bool()($normalized_unique_name_7)" ^
  "\n77:31:goto 76 if not $normalized_unique_name_8" ^
  "\n66:31:print(i) > " ^
  "\n75:31:goto 74" ^
  "\n76:31:pass" ^
  "\n74:31:pass" ^
  "\n81:31:goto 42" ^
  "\n43:31:pass" ^
  "\n111::goto 32" ^
  "\n31::catch($normalized_unique_name_9)" ^
  "\n87::$normalized_unique_name_10=type($normalized_unique_name_9)" ^
  "\n91::$normalized_unique_name_11=$normalized_unique_name_10.__eq__" ^
  "\n94::$normalized_unique_name_12=$normalized_unique_name_11(StopIteration)" ^
  "\n104::$normalized_unique_name_13=bool()($normalized_unique_name_12)" ^
  "\n109::goto 108 if not $normalized_unique_name_13" ^
  "\n96::pass" ^
  "\n107::goto 106" ^
  "\n108::pass" ^
  "\n98::raise($normalized_unique_name_9)" ^
  "\n106::pass" ^
  "\n32::pass"


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
