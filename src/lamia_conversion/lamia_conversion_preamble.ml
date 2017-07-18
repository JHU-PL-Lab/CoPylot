open Batteries;;
open Lamia_ast;;
open Lamia_conversion_monad;;
open Conversion_monad;;
open Lamia_conversion_builtin_names;;
open Lamia_conversion_builtin_defs;;

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

let all_definitions =
  [
    define_scope;
    define_func_val get_from_scope_def get_from_scope;
    define_get_from_parent_scope;

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
