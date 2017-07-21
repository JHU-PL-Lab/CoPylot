open Batteries;;
open Lamia_ast;;
open Lamia_conversion_monad;;
open Conversion_monad;;
open Lamia_conversion_builtin_names;;
open Lamia_conversion_builtin_defs;;
open Lamia_conversion_object_defs;;

let define_func_mem body_def func_loc =
  let%bind arglist = fresh_value_var () in

  let%bind _, func_body = listen @@ body_def arglist in

  let%bind func_name = fresh_value_var () in
  let func_val = Function_expression([arglist], Block(func_body)) in
  emit
    [
      Let_alloc(func_loc);
      Let_expression(func_name, func_val);
      Store(func_loc, func_name);
    ]
;;

let define_func_val func func_name =
  let%bind func_expr = func in
  emit
    [
      Let_expression(func_name, func_expr);
    ]
;;

let define_obj_mem obj_loc fill_func starvalue =
  let%bind value_val = fresh_value_var () in
  let%bind value_loc = fresh_memory_var () in
  let%bind empty_obj = fresh_value_var () in
  let%bind value_str = fresh_value_var () in
  let%bind obj_with_value = fresh_value_var () in
  let%bind _ =
    emit
      [
        Let_alloc(value_loc);
        Let_expression(value_val, starvalue);
        Store(value_loc, value_val);

        Let_expression(empty_obj, Empty_binding);
        Let_expression(value_str, String_literal "*value");
        Let_binding_update(obj_with_value, empty_obj, value_str, value_loc);
      ]
  in
  let%bind filled_obj = fill_func obj_with_value in
  emit
    [
      Let_alloc(obj_loc);
      Store(obj_loc, filled_obj);
    ]
;;

let all_definitions =
  [
    define_scope;
    define_func_val get_from_scope_def get_from_scope;
    define_get_from_parent_scope;

    define_obj_mem builtin_true fill_bool @@ Integer_literal 1;
    define_obj_mem builtin_false fill_bool @@ Integer_literal 0;
    define_obj_mem builtin_none fill_none @@ None_literal;

    (* define_func_mem builtin_attribute_error_body builtin_AttributeError;
       define_func_mem builtin_type_error_body builtin_TypeError;
       define_func_mem builtin_name_error_body builtin_NameError;
       define_func_mem builtin_stop_iteration_body builtin_StopIteration; *)

    (* define_func_mem builtin_bool_body builtin_bool; *)

    define_func_mem int_add_body int_add;
    (* define_func_mem method_call_body method_call; *)
  ]

let preamble =
  sequence all_definitions
;;
