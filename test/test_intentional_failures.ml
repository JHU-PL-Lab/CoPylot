open OUnit2
open Batteries
open Jhupllib
open Python2_parser
module Concrete = Python2_ast
open Lexing
open Python2_abstract_ast
module Convert = Python2_analysis_conversion

let annot = Concrete.Pos.of_pos Lexing.dummy_pos;;

let string_of_stmt e  = Pp_utils.pp_to_string
    (Pp_utils.pp_list (Python2_abstract_ast.pp_stmt (fun _ _ -> ()))) e;;
let equivalent_stmt e1 e2 = List.eq (equal_stmt ( fun _ _ -> true)) e1 e2;;

let string_of_modl m = Pp_utils.pp_to_string
    (Python2_abstract_ast.pp_modl (fun _ _ -> ())) m;;
let equivalent_modl m1 m2 = equal_modl ( fun _ _ -> true) m1 m2;;

let parse_stmt_from_string_safe str =
  try
    parse_stmt_from_string str
  with
  | Python2_parser.Parse_error p ->
    assert_failure (Printf.sprintf "Error in line %d, col %d."
                      p.pos_lnum p.pos_cnum)
;;

let parse_from_string_safe str =
  try
    parse_from_string str
  with
  | Python2_parser.Parse_error p ->
    assert_failure (Printf.sprintf "Error in line %d, col %d."
                      p.pos_lnum p.pos_cnum)
;;

(*
let prog = "1" in
let concrete = parse_stmt_from_string_safe (prog ^ "\n") in
let actual = List.map Convert.lift_stmt concrete in
print_endline (string_of_stmt actual)
;;
*)

(* Functions to hide testing boilerplate *)

let gen_module_test (name : string) (prog : string) (expected : 'a stmt list) =
  name>::
  ( fun _ ->
      let concrete = parse_from_string_safe (prog ^ "\n") in
      let actual = Convert.lift_modl concrete in
      assert_equal ~printer:string_of_modl ~cmp:equivalent_modl
        (Module(expected, annot)) actual
  )

let gen_stmt_test (name : string) (prog : string) (expected : 'a expr) =
  name>::
  ( fun _ ->
      let concrete = parse_stmt_from_string_safe (prog ^ "\n") in
      let actual = List.map Convert.lift_stmt concrete in
      assert_equal ~printer:string_of_stmt ~cmp:equivalent_stmt
        [(Expr(expected, annot))] actual
  )

let bad_names =
  "def f(x):" ^
  "\n\tif x==1:" ^
  "\n\t\tdef f(f):" ^
  "\n\t\t\treturn f(x-1)" ^
  "\n\t\tdef x(x):" ^
  "\n\t\t\treturn x" ^
  "\n\t\treturn f(x)" ^
  "\n\telse:" ^
  "\n\t\treturn f(x-1)"
;;

let nice_names =
  "def f(x):" ^
  "\n\tif x==1:" ^
  "\n\t\tdef g(z):" ^
  "\n\t\t\treturn z(x-1)" ^
  "\n\t\tdef h(y):" ^
  "\n\t\t\treturn y" ^
  "\n\t\treturn g(h)" ^
  "\n\telse:" ^
  "\n\t\treturn f(x-1)"
;;

let inner_access =
  "x=False" ^
  "\ndef f():" ^
  "\n\tif x:" ^
  "\n\t\treturn True" ^
  "\n\treturn False"
;;

let bad_assignment =
  "x=False" ^
  "\ndef f():" ^
  "\n\tif x:" ^
  "\n\t\treturn True" ^
  "\n\tx=5" ^
  "\n\tpass"
;;

let var_scope =
  "def f(x):" ^
  "\n\tx=1" ^
  "\n\treturn x" ^
  "\nx=f(2)"
;;

let new_x =
  "def f(x):" ^
  "\n\ty=x" ^
  "\n\tx=1"
;;

let evil_f =
  "def f(f):" ^
  "\n\tdef f():" ^
  "\n\t\treturn f" ^
  "\n\treturn f" ^
  "\nf=f(f)"
;;

let evil_f2 =
  "def f():" ^
  "\n\tdef f():" ^
  "\n\t\treturn f" ^
  "\n\treturn f" ^
  "\nf=f()"

let augment =
  "x+=1"
;;

let param_augment =
  "def f(x):" ^
  "\n\tx+=1"
;;

let param_new =
  "def f(x):" ^
  "\n\tx=1"
;;

let fun_assignment =
  "def f():" ^
  "\n\treturn 1" ^
  "\ndef g(x):" ^
  "\n\tx=f" ^
  "\n\treturn x" ^
  "\nx=g(g)"

let missing_global =
  "def f(x):" ^
  "\n\tif y:" ^
  "\n\t\tx=0" ^
  "\n\telse:" ^
  "\n\t\ty=x" ^
  "\ny=1"

let list_context =
  "a=[4,5]" ^
  "\ndef f():" ^
  "\n\treturn a[-1]"

let redefine_function =
  "f=0" ^
  "\ndef f():" ^
  "\n\treturn g" ^
  "\ndef g(x):" ^
  "\n\treturn x()" ^
  "\ng(f)"
;;

let module_dummy = [
  Assign(
    [ Name("x", Store, annot) ],
    Num(Int(Pos), annot),
    annot
  )
]
;;

let stmt_dummy = (Num(Int(Pos), annot))
;;

let modl_test = gen_module_test "modl_test"
    list_context
    module_dummy
;;

(* Run the tests *)

let tests =
  "abstract_ast">:::
  [
    modl_test
  ]
