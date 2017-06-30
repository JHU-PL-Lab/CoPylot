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

let gen_variable_test (name : string) (prog : string)
    (target: string) (expected : value) =
  name>::
  ( fun _ ->
      let normalized = parse_to_normalized_safe prog 0 0 true in
      Python2_ast_simplifier.reset_unique_name ();

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

let starting_bindings = Bindings.empty;;
let empty_state =
  {
    micro = Micro_instruction_stack.empty;
    stack = Program_stack.empty;
    heap = Heap.singleton (Memloc(0)) (Bindings(starting_bindings))
  }
;;

let literal_tests = [
  gen_variable_test "int_test_pos" "x = 4" "x" @@ Num(Int(4));
  gen_variable_test "int_test_neg" "x = -4" "x" @@ Num(Int(-4));
  gen_variable_test "int_test_zero" "x = 0" "x" @@ Num(Int(0));

  gen_variable_test "float_test_pos" "x = 4.0" "x" @@ Num(Float(4.0));
  gen_variable_test "float_test_neg" "x = -4.0" "x" @@ Num(Float(-4.0));
  gen_variable_test "float_test_zero" "x = 0.0" "x" @@ Num(Float(0.0));

  gen_variable_test "string_test" "x = \"foo\"" "x" @@ Str(StringLiteral("foo"));

  gen_variable_test "list_test" "x = []" "x" @@ ListVal([]);
  gen_variable_test "tuple_test" "x = (None,)" "x" @@ TupleVal([None_memloc]);
]

let alias_test_light = gen_variable_test "alias_test_light"
    "x = 4; y = x; z = y; y = 5;"
    "z"
  @@ Num(Int(4))
;;

let tests =
  "test_pys_interpreter">:::
  literal_tests @
  [
    alias_test_light;
  ]
