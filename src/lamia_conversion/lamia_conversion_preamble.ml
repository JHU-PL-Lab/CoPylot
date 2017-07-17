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

let define_scope =
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

let define_get_from_parent_scope =
  let%bind target_name = fresh_value_var () in
  let%bind _, throw_exn =
    listen @@
    let%bind exn_val =
      (* FIXME: The error message should by dynamically constructed to
         hold the name we were looking up *)
      store_value @@ String_literal("name is not defined")
    in
    let%bind exn_obj = wrap_name_error exn_val in
    emit
      [
        Raise(exn_obj);
      ]
  in
  emit
    [
      Let_expression(get_from_parent_scope,
                     Function_expression(
                       [target_name],
                       Block(throw_exn)));
    ]
;;

(* TODO: This should probably go in a separate file where we put all the
   definitions together *)
let define_func body_def func_loc =
  let%bind arglist = fresh_value_var () in

  let%bind _, func_body = listen @@ body_def arglist in

  let%bind func_name = fresh_value_var () in
  let func_val = Function_expression([arglist], Block(func_body)) in
  emit
    [
      Let_expression(func_name, func_val);
      Store(func_loc, func_name);
    ]
;;

(*TODO: All the builtin functions need arglist size verification and/or
  typechecking *)

let builtin_attribute_error_body arglist =
  let%bind arg_val = extract_arg_to_value arglist 0 in
  wrap_attribute_error arg_val
;;

let builtin_type_error_body arglist =
  let%bind arg_val = extract_arg_to_value arglist 0 in
  wrap_type_error arg_val
;;

let builtin_name_error_body arglist =
  let%bind arg_val = extract_arg_to_value arglist 0 in
  wrap_name_error arg_val
;;

let builtin_stop_iteration_body arglist =
  let%bind arg_val = extract_arg_to_value arglist 0 in
  wrap_stop_iteration arg_val
;;

let builtin_bool_body arglist =
  let%bind arg_loc = extract_nth_list_elt arglist 0 in
  let%bind arg_obj = get_value arg_loc in
  let%bind val_loc = get_attr "*value" arg_obj in
  let%bind arg_val = get_value val_loc in

  let check_starvalue toplevel argval test_expr rest =
    let%bind _, rest_of_if = listen rest in
    let%bind test_name = fresh_value_var () in
    let%bind value_name = fresh_value_var () in
    let%bind result_name = fresh_value_var () in
    let%bind _, return_false =
      listen @@
      emit
        [
          If_result_value(builtin_false);
        ]
    in
    let return_if_necessary =
      if toplevel then
        [If_result_value(result_name)]
      else
        []
    in
    let%bind _ =
      emit @@
      [
        Let_expression(value_name, test_expr);
        Let_binop(test_name, value_name, Binop_equals, argval);
        Let_conditional_value(result_name, test_name, Block(return_false), Block(rest_of_if));
      ] @
      return_if_necessary
    in
    return result_name
  in

  (* Check if it has a __bool__ or __len__ method, and call it *)
  let call_bool_and_len =

    let%bind bool_name = fresh_value_var () in
    let%bind len_name = fresh_value_var () in

    let check_haskey key_str key_name if_yes if_no =
      let%bind has_key = fresh_value_var () in
      let%bind retval = fresh_value_var () in
      emit
        [
          Let_expression(key_name, String_literal(key_str));
          Let_binop(has_key, arg_obj, Binop_haskey, key_name);
          Let_conditional_value(retval, has_key, Block if_yes, Block if_no);
          If_result_value(retval);
        ]
    in

    let call_method method_name =
      let%bind method_loc = fresh_memory_var () in
      let%bind _ = emit [Let_binding_access(method_loc, arg_obj, method_name)] in
      let%bind method_func = get_call method_loc in
      let%bind method_func_result = fresh_memory_var () in
      let%bind method_func_arglist = fresh_value_var () in
      let%bind method_func_result_value = fresh_value_var () in
      let%bind _ =
        emit
          [
            Let_expression(method_func_arglist, List_value [arg_loc]);
            Let_call_function(method_func_result, method_func, [method_func_arglist]);
            Let_get(method_func_result_value, method_func_result);
          ]
      in
      return method_func_result_value
    in

    let%bind _, if_has_bool =
      listen @@
      let%bind bool_result = call_method bool_name in
      emit
        [
          (* TODO: Make sure result of __bool__ really is a bool *)
          If_result_value(bool_result);
        ]
    in

    let%bind _, if_no_bool = listen @@
      let return_true = emit [If_result_value(builtin_true)] in

      let%bind _, if_has_len =
        listen @@
        let%bind len_result = call_method len_name in
        let%bind len_result_value_loc = get_attr "*value" len_result in
        let%bind len_result_value = get_value len_result_value_loc in
        let%bind len_result_bool = fresh_value_var () in
        let%bind _, if_not_int =
          listen @@
          emit
            [
              (* TODO: Throw error of some kind *)
            ]
        in
        let%bind isint = fresh_value_var () in
        let%bind _, if_is_int =
          listen @@
          check_starvalue false len_result_value (Integer_literal 0) return_true
        in
        emit
          [
            Let_unop(isint, Unop_is_int, len_result_value);
            Let_conditional_value(len_result_bool, isint, Block if_is_int, Block if_not_int);
            If_result_value(len_result_bool);
          ]
      in

      let%bind _, if_no_len =
        listen @@ return_true
      in

      check_haskey "__len__" len_name if_has_len if_no_len

    in

    check_haskey "__bool__" bool_name if_has_bool if_no_bool
  in

  let%bind bool_result =
    check_starvalue true arg_val (None_literal) @@
    check_starvalue false arg_val (Boolean_literal false) @@
    check_starvalue false arg_val (Integer_literal 0) @@
    (* check_starvalue (Float_literal 0.0) @@ *)
    check_starvalue false arg_val (String_literal "") @@
    check_starvalue false arg_val (List_value []) @@
    check_starvalue false arg_val (Empty_binding) @@
    call_bool_and_len
  in

  let%bind obj_result = wrap_bool bool_result in
  emit
    [
      Return(obj_result);
    ]
;;

let int_add_body arglist =
  let%bind arg0_val = extract_arg_to_value arglist 0 in
  let%bind arg1_val = extract_arg_to_value arglist 1 in
  let%bind sum = fresh_value_var () in
  let%bind _ = emit
      [
        Let_binop(sum, arg0_val, Binop_intplus, arg1_val);
      ]
  in
  let%bind obj_result = wrap_int sum in
  emit
    [
      Return(obj_result);
    ]
;;

let method_call_body arglist =
  let%bind method_val = extract_arg_to_value arglist 0 in
  let%bind method_func = get_attr "*value" method_val in
  let%bind method_func_val = get_value method_func in

  let%bind start_index = fresh_value_var () in
  let%bind end_index = fresh_value_var () in
  let%bind truncated_args = fresh_value_var () in
  let%bind retval = fresh_memory_var () in
  emit
    [
      (* Remove the first element from the arglist, since it's the "self"
         for __call__, not for the function __call__ invokes *)
      Let_expression(start_index, Integer_literal 1);
      Let_expression(end_index, None_literal);
      Let_list_slice(truncated_args, arglist, start_index, end_index);
      (* Call the function *)
      Let_call_function(retval, method_func_val, [truncated_args]);
      Return(retval);
    ]
;;
