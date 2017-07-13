(* open Batteries;; *)
open Lamia_ast;;
open Lamia_conversion_monad;;
open Conversion_monad;;
open Lamia_conversion_builtin_names;;
open Lamia_conversion_object_defs;;
open Lamia_conversion_utils;;

(* This file contains all definitions of python builtins, global helper
   functions like *getcall, etc *)

let annot = Python2_ast.Pos.of_pos Lexing.dummy_pos;;

let scope_def =
  let%bind empty_name = fresh_value_var () in
  emit
    [
      Let_expression(empty_name, Empty_binding);
      Let_alloc(python_scope);
      Store(python_scope, empty_name);
    ]
;;

let get_from_scope_def =
  let%bind target = fresh_value_var () in
  let%bind _, func_body =
    listen @@
    let lookup_in_parent =
      let%bind parent_retval = fresh_memory_var () in
      emit
        [
          Let_call_function(parent_retval, get_from_parent_scope, [target]);
          If_result_memory(parent_retval);
        ]
    in
    let%bind scope_val = fresh_value_var () in
    let%bind local_result =
      get_from_binding target scope_val lookup_in_parent
    in
    emit
      [
        Let_get(scope_val, python_scope);
        If_result_memory(local_result);
      ]
  in
  return @@ Function_expression([target], Block(func_body))
;;

let get_from_parent_scope_def =
  let%bind target_name = fresh_value_var () in
  emit
    [
      Let_expression(get_from_parent_scope,
                     Function_expression(
                       [target_name],
                       Block
                         [
                           (* TODO: allocate and throw a NameError *)
                         ]));
    ]
;;

let int_add_def =
  let%bind arglist = fresh_value_var () in

  let%bind _, func_body =
    listen @@
    let%bind arg0_val = extract_arg_to_value arglist 0 in
    let%bind arg1_val = extract_arg_to_value arglist 1 in
    let%bind sum = fresh_value_var () in
    let%bind _ = emit
        [
          (* TODO: Typechecking *)
          Let_binop(sum, arg0_val, Binop_intplus, arg1_val);
        ]
    in
    let%bind obj_result = wrap_int sum in
    emit
      [
        Return(obj_result);
      ]
  in

  let%bind func_name = fresh_value_var () in
  let func_val = Function_expression([arglist], Block(func_body)) in
  emit
    [
      Let_expression(func_name, func_val);
      Store(int_add, func_name);
    ]
;;

(* TODO: Define function values *)
(* TODO: Store in global memlocs *)
