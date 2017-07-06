open OUnit2;;
open Batteries;;
open Jhupllib;;

open Python2_ast_types;;
open Python2_ast_pipeline;;

open Python2_pys_interpreter_types;;
open Python2_pys_interpreter;;

let string_of_program_state s = Pp_utils.pp_to_string pp_program_state s;;
let string_of_value v = Pp_utils.pp_to_string pp_value v;;

let parse_to_normalized_safe prog short_names =
  try
    parse_to_normalized prog short_names
  with
  | Python2_parser.Parse_error p ->
    let open Lexing in
    assert_failure (Printf.sprintf "Error in line %d, col %d."
                      p.pos_lnum p.pos_cnum)
;;

let parse_pyssembly_safe prog =
  try
    Python2_pys_parser.parse_from_string prog
  with
  | Python2_pys_parser.Parse_error loc ->
    assert_failure (Printf.sprintf "Error at location %d" loc)
;;

let gen_module_test (name : string) (prog : string)
    (expected : program_state ) =
  name>::
  ( fun _ ->
      let actual = parse_to_normalized_safe prog 0 0 true in
      Python2_ast_simplifier.reset_unique_name ();

      let result_state = interpret_program actual in
      assert_equal ~printer:string_of_program_state ~cmp:equal_program_state expected result_state
  )
;;

let gen_variable_test (name : string) (prog: string)
    (parse_func: string -> Python2_normalized_ast.modl)
    (target: string) (expected : value) =
  name>::
  ( fun _ ->
      let normalized = parse_func prog in
      let result_state = interpret_program normalized in
      let open Python2_pys_interpreter_utils in
      let var_memloc =
        retrieve_binding_or_fail result_state.heap Python2_pys_interpreter_init.global_memloc
        |> Bindings.get_memloc target
      in
      let var_memloc =
        extract_option_or_fail var_memloc @@
        "Requested variable" ^ target ^ "not bound"
      in
      let value_memloc =
        retrieve_binding_or_fail result_state.heap var_memloc
        |> Bindings.get_memloc "*value"
      in
      let value_memloc = extract_option_or_fail value_memloc @@
        "Requested variable" ^ target ^ "not bound"
      in
      let actual = Heap.get_value value_memloc result_state.heap in
      assert_equal ~printer:string_of_value ~cmp:equal_value expected actual
  )
;;

let gen_python_variable_test (name : string) (prog : string)
    (target: string) (expected : value) =
  let parse_func =
    (fun p ->
       let normalized = parse_to_normalized_safe p 0 0 true in
       Python2_ast_simplifier.reset_unique_name ();
       normalized)
  in
  gen_variable_test name prog parse_func target expected
;;

let gen_pyssembly_variable_test (name : string) (prog : string)
    (target: string) (expected : value) =
  gen_variable_test name prog parse_pyssembly_safe target expected
;;

let starting_bindings = Bindings.empty;;
let empty_state =
  {
    micro = Micro_instruction_stack.empty;
    stack = Program_stack.empty;
    heap = Heap.singleton (Memloc(0)) (Bindings(starting_bindings))
  }
;;

let literal_tests = [
  gen_python_variable_test "int_test_pos" "x = 4" "x" @@ Num(Int(4));
  gen_python_variable_test "int_test_neg" "x = -4" "x" @@ Num(Int(-4));
  gen_python_variable_test "int_test_zero" "x = 0" "x" @@ Num(Int(0));

  gen_python_variable_test "float_test_pos" "x = 4.0" "x" @@ Num(Float(4.0));
  gen_python_variable_test "float_test_neg" "x = -4.0" "x" @@ Num(Float(-4.0));
  gen_python_variable_test "float_test_zero" "x = 0.0" "x" @@ Num(Float(0.0));

  (* gen_python_variable_test "bool_test" "x = True" "x" @@ Bool(true); *)

  gen_python_variable_test "string_test" "x = \"foo\"" "x" @@ Str(StringLiteral("foo"));

  gen_python_variable_test "list_test" "x = []" "x" @@ ListVal([]);
  gen_python_variable_test "tuple_test" "x = (None,)" "x" @@ TupleVal([None_memloc]);
]
;;

let operator_tests = [
  gen_pyssembly_variable_test "int_add_test" "x=1;y=2;attr=\"__add__\";add=x.attr;z=add(y);" "z" @@ Num(Int(3));
  gen_pyssembly_variable_test "int_neg_test" "x=1;attr=\"__neg__\";neg=x.attr;z=neg();" "z" @@ Num(Int(-1));
  gen_pyssembly_variable_test "str_add_test" "x=\"foo\";y=\"bar\";attr=\"__add__\";add=x.attr;z=add(y);" "z" @@ Str(StringLiteral "foobar");
  gen_pyssembly_variable_test "str_contains_test" "x=\"abcdef\";y=\"bcd\";attr=\"__contains__\";contains=x.attr;z=contains(y);" "z" @@ Bool(true);
  gen_pyssembly_variable_test "list_getitem_test1" "a=0;b=1;c=2;d=\"three\";x=[a,b,c,d];y=2;attr=\"__getitem__\";getitem=x.attr;z=getitem(y);" "z" @@ Num(Int(2));
  gen_python_variable_test "real_int_add_test" "z = 1+2" "z" @@ Num(Int(3));
]
;;

let alias_test_light = gen_python_variable_test "alias_test_light"
    "x = 4; y = x; z = y; y = 5;"
    "z"
  @@ Num(Int(4))
;;

let tests =
  "test_pys_interpreter">:::
  literal_tests @
  operator_tests @
  [
    alias_test_light;
  ]
