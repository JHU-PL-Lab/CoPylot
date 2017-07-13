open Batteries;;
open Lamia_ast;;
open Lamia_conversion_monad;;
open Conversion_monad;;
open Lamia_conversion_builtin_names;;

(* let map_and_concat (func : 'a -> 'b list) (lst : 'a list) =
   List.concat (List.map func lst)
   ;; *)

let fresh_value_var () : value_variable m =
  let%bind name = fresh_name () in
  return @@ Value_variable(name)
;;

let fresh_memory_var () : memory_variable m =
  let%bind name = fresh_name () in
  return @@ Memory_variable(name)
;;

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

let get_attr target bindings =
  let%bind target_result = store_value @@ String_literal(target) in
  let%bind exn_loc = fresh_memory_var () in
  let alloc_exn =
    ignore exn_loc;
    emit
      [
        (* TODO: Alloc & throw AttributeError *)
      ]
  in
  get_from_binding target_result bindings alloc_exn
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
