open Batteries;;
open Lamia_ast;;
open Lamia_ast_types;;
open Lamia_conversion_monad;;
open Conversion_monad;;
open Lamia_conversion_builtin_names;;
open Lamia_conversion_object_defs;;

let store_value
    (v : annot value_expression) =
  let%bind value_name = fresh_value_var () in
  let%bind _ = emit
      [
        Let_expression(value_name, v);
      ]
  in
  return value_name
;;

let get_value value_loc =
  let%bind value_result = fresh_value_var () in
  let%bind _ = emit
      [
        Let_get(value_result, value_loc);
      ]
  in
  return value_result
;;

(* Get the memloc with key id from the binding at binding_loc. If no such
   memloc exists, run the directives in on_failure. *)
let get_from_binding target binding_val on_failure =
  let%bind haskey = fresh_value_var () in
  let%bind retval = fresh_memory_var () in

  let%bind _, on_success =
    listen @@
    let%bind ret_memloc = fresh_memory_var () in
    emit
      [
        Let_binding_access(ret_memloc, binding_val, target);
        If_result_memory(ret_memloc);
      ]
  in
  let%bind _, on_failure = listen on_failure in
  let success_block = Block on_success in
  let fail_block = Block on_failure in

  let%bind _ = emit
      [
        Let_binop(haskey, binding_val, Binop_haskey, target);
        Let_conditional_memory(retval, haskey, success_block, fail_block);
      ]
  in
  return retval
;;

let lookup (id : string) =
  let%bind target_result = store_value @@ String_literal(id) in
  let%bind retval = fresh_memory_var () in
  let%bind _ = emit
      [
        Let_call_function(retval, get_from_scope, [target_result]);
      ]
  in
  return retval
;;

let get_or_error target bindings on_failure =
  let%bind target_result = store_value @@ String_literal(target) in
  get_from_binding target_result bindings on_failure
;;

let get_attr target bindings =
  let throw_exn =
    (* FIXME: The error string should by dynamically constructed to
       hold the class of the object *)
    let%bind exn_val =
      store_value @@ String_literal("Object has no attribute " ^ target)
    in
    let%bind exn_obj = wrap_attribute_error exn_val in
    emit
      [
        Raise(exn_obj);
      ]
  in
  get_or_error target bindings throw_exn
;;

let lookup_and_get_attr varname attr =
  let%bind lookup_result = lookup varname in
  let%bind obj_val = fresh_value_var () in
  let%bind _ = emit
      [
        Let_get(obj_val, lookup_result)
      ]
  in
  get_attr attr obj_val
;;

let assign_python_variable id y =
  let%bind varname = fresh_value_var () in
  let%bind old_scopeval = fresh_value_var () in
  let%bind new_scopeval = fresh_value_var () in
  emit
    [
      Let_expression(varname, String_literal id);
      Let_get(old_scopeval, python_scope);
      Let_binding_update(new_scopeval, old_scopeval, varname, y);
      Store(python_scope, new_scopeval);
    ]
;;

let convert_list
    (convert_func : 'a -> 'b m)
    (lst :'a list)
  : 'b list m =
  let accumulate elt m =
    let%bind new_result = convert_func elt in
    let%bind old_results = m in
    return @@ new_result::old_results
  in
  List.fold_right accumulate lst (return [])
;;

let extract_nth_list_elt listname n =
  let%bind index = fresh_value_var () in
  let%bind value = fresh_memory_var () in
  let%bind _ = emit
      [
        Let_expression(index, Integer_literal n);
        Let_list_access(value, listname, index);
      ]
  in
  return value
;;

let extract_arg_to_value arglist n =
  let%bind arg_loc = extract_nth_list_elt arglist n in
  let%bind arg_obj = get_value arg_loc in
  let%bind val_loc = get_attr "*value" arg_obj in
  let%bind arg_val = get_value val_loc in
  return arg_val
;;

(* Given an object, get a function we can call (or throw an exception)
   pseudocode: while target.*value is not a function, target = target.__call__
*)
let get_call target =
  (* Store target in a new, temporary variable *)
  let%bind tmp_loc = fresh_memory_var () in
  let%bind _ =
    emit
      [
        Let_alias_memory(tmp_loc, target);
      ]
  in
  (* Get its *value field *)
  (* TODO: Catch attribute error and continue *)
  let%bind test_loc = fresh_memory_var () in
  let%bind tmp_bindings = fresh_value_var () in
  let%bind tmp_value = fresh_value_var () in
  let%bind _, compute_test =
    listen @@
    let%bind _ = emit [Let_get(tmp_bindings, tmp_loc)] in
    let%bind tmp_value_loc = get_attr "*value" tmp_bindings in
    let%bind _ = emit [Let_get(tmp_value, tmp_value_loc)] in
    let%bind test_name = fresh_value_var () in
    let%bind test_name_inverted = fresh_value_var () in
    emit
      [
        Let_unop(test_name, Unop_is_function, tmp_value);
        Let_unop(test_name_inverted, Unop_not, test_name);
        Let_alloc(test_loc);
        Store(test_loc, test_name_inverted);
      ]
  in

  (* Body of the while loop: assign tmp to tmp.__call__ *)
  let%bind _, while_body =
    listen @@
    let throw_exn =
      (* FIXME: The error string should by dynamically constructed to
         hold the class of the object *)
      let%bind exn_val =
        store_value @@ String_literal("Object is not callable")
      in
      let%bind exn_obj = wrap_type_error exn_val in
      emit
        [
          Raise(exn_obj);
        ]
    in
    let%bind new_tmp = get_or_error "__call__" tmp_bindings throw_exn in
    emit
      [
        Let_alias_memory(tmp_loc, new_tmp);
      ]
  in

  (* Put everything together *)
  let compute_test_directives =
    List.map (fun s -> let Statement(_, d) = s in d) compute_test
  in
  let%bind _ =
    emit @@
    compute_test_directives @
    [
      While(test_loc, Block(while_body @ compute_test));
    ]
  in
  return tmp_value
;;
