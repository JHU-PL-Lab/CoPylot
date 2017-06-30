open OUnit2;;
open Batteries;;
open Jhupllib;;

open Python2_ast_types;;
open Python2_ast_pipeline;;

open Python2_pys_interpreter_types;;
open Python2_pys_interpreter;;

let string_of_program_state s = Pp_utils.pp_to_string pp_program_state s;;

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
      let actual = parse_to_normalized_safe prog true false in
      Python2_ast_simplifier.reset_unique_name ();
      Python2_ast_normalizer.reset_unique_name ();

      let result_state = interpret_program actual in
      assert_equal ~printer:string_of_program_state ~cmp:equal_program_state expected result_state
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

let pass_test = gen_module_test "pass_test"
    "pass"
    empty_state
;;

let int_test = gen_module_test "int_test"
    "4"
    { empty_state with heap =
                         Heap.empty
                         |> Heap.update_binding (Memloc(0))
                           begin
                             Bindings(
                               starting_bindings
                               |> Bindings.update_binding "$norm0" (Memloc(2))
                             )
                           end
                         |> Heap.update_binding (Memloc(1)) (Num(Int(4)))
                         |> Heap.update_binding (Memloc(2))
                           begin
                             Bindings(
                               starting_bindings
                               |> Bindings.update_binding "*value" (Memloc(1))
                               |> Bindings.update_binding "__class__" (Builtin_type_memloc(Int_type))
                               |> Bindings.update_binding "__add__" (Builtin_fun_memloc(Builtin_int_add))
                               |> Bindings.update_binding "__neg__" (Builtin_fun_memloc(Builtin_int_neg))
                             )
                           end
    }
;;

let int_assign_test = gen_module_test "int_assign_test"
    "x = 4"
    { empty_state with heap =
                         Heap.empty
                         |> Heap.update_binding (Memloc(0))
                           begin
                             Bindings(
                               starting_bindings
                               |> Bindings.update_binding "$norm0" (Memloc(2))
                               |> Bindings.update_binding "x" (Memloc(2))
                               |> Bindings.update_binding "$simp0" (Memloc(2))
                             )
                           end
                         |> Heap.update_binding (Memloc(1)) (Num(Int(4)))
                         |> Heap.update_binding (Memloc(2))
                           begin
                             Bindings(
                               starting_bindings
                               |> Bindings.update_binding "*value" (Memloc(1))
                               |> Bindings.update_binding "__class__" (Builtin_type_memloc(Int_type))
                               |> Bindings.update_binding "__add__" (Builtin_fun_memloc(Builtin_int_add))
                               |> Bindings.update_binding "__neg__" (Builtin_fun_memloc(Builtin_int_neg))
                             )
                           end
    }
;;

let tests =
  "test_pys_interpreter">:::
  [
    pass_test;
    int_test;
    int_assign_test;
  ]
