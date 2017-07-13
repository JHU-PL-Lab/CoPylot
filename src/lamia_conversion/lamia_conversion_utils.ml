open Batteries;;
open Lamia_ast;;
open Lamia_conversion_monad;;
open Conversion_monad;;
open Lamia_conversion_builtin_names;;
open Lamia_conversion_ctx;;

let map_and_concat (func : 'a -> 'b list) (lst : 'a list) =
  List.concat (List.map func lst)
;;

let fresh_value_var () : value_variable m =
  let%bind name = fresh_name () in
  return @@ Value_variable(name)
;;

let fresh_memory_var () : memory_variable m =
  let%bind name = fresh_name () in
  return @@ Memory_variable(name)
;;

let store_value ctx annot
    (v : annot value_expression) =
  let value_name = Value_variable(gen_unique_name ctx annot) in
  [
    annotate_directive ctx annot @@
    Let_expression(value_name, v);
  ],
  value_name
;;

let get_value ctx annot value_loc =
  let value_result = Value_variable(gen_unique_name ctx annot) in
  [
    annotate_directive ctx annot @@
    Let_get(value_result, value_loc);
  ],
  value_result
;;

(* Get the memloc with key id from the binding at binding_loc. If no such
   memloc exists, run the directives in on_failure. *)
let get_from_binding ctx annot target binding_val on_failure =
  let haskey = Value_variable(gen_unique_name ctx annot) in
  let retval = Memory_variable(gen_unique_name ctx annot) in

  let success =
    let ret_memloc = Memory_variable(gen_unique_name ctx annot) in
    [
      Let_binding_access(ret_memloc, binding_val, target);
      If_result_memory(ret_memloc);
    ]
  in
  let fail = on_failure
  in
  let success_block =
    Block (List.map (annotate_directive ctx annot) success)
  in
  let fail_block =
    Block (List.map (annotate_directive ctx annot) fail)
  in

  List.map (annotate_directive ctx annot)
    [
      Let_binop(haskey, binding_val, Binop_haskey, target);
      Let_conditional_memory(retval, haskey, success_block, fail_block);
    ],
  retval
;;

let lookup ctx annot (id : string) =
  let target_bindings, target_result =
    store_value ctx annot @@ String_literal(id)
  in
  let retval = Memory_variable(gen_unique_name ctx annot) in
  target_bindings @
  [
    annotate_directive ctx annot @@
    Let_call_function(retval, get_from_scope, [target_result]);
  ],
  retval
;;

let get_attr ctx annot target bindings =
  let target_bindings, target_result =
    store_value ctx annot @@ String_literal(target)
  in
  let exn_loc = Memory_variable(gen_unique_name ctx annot) in
  let alloc_exn =
    ignore exn_loc;
    [
      (* TODO: Alloc & throw AttributeError *)
    ]
  in
  let extraction, extraction_result =
    get_from_binding ctx annot target_result bindings alloc_exn
  in
  target_bindings @ extraction,
  extraction_result
;;

let lookup_and_get_attr ctx annot varname attr =
  let lookup_bindings, lookup_result = lookup ctx annot varname in
  let obj_val = Value_variable(gen_unique_name ctx annot) in
  let extract_obj_val =
    [
      annotate_directive ctx annot @@
      Let_get(obj_val, lookup_result)
    ]
  in
  let attr_bindings, attr_result =
    get_attr ctx annot attr obj_val
  in
  lookup_bindings @ extract_obj_val @ attr_bindings,
  attr_result
;;

let assign_python_variable ctx annot id y =
  let scope_update_directives =
    let varname = Value_variable(gen_unique_name ctx annot) in
    let old_scopeval = Value_variable(gen_unique_name ctx annot) in
    let new_scopeval = Value_variable(gen_unique_name ctx annot) in
    [
      Let_expression(varname, String_literal id);
      Let_get(old_scopeval, python_scope);
      Let_binding_update(new_scopeval, old_scopeval, varname, y);
      Store(python_scope, new_scopeval);
    ]
  in
  let scope_update_stmts =
    List.map (annotate_directive ctx annot) scope_update_directives
  in
  scope_update_stmts
;;

let convert_list
    (convert_func : 'a -> 'b m)
    (lst :'a list)
  : 'b list m =
  let accumulate m elt =
    let%bind new_result = convert_func elt in
    let%bind old_results = m in
    return @@ new_result::old_results
  in
  List.fold_right accumulate lst (return [])
;;

let extract_nth_list_elt ctx annot listname n =
  let index = Value_variable(gen_unique_name ctx annot) in
  let value = Memory_variable(gen_unique_name ctx annot) in
  let directives =
    [
      Let_expression(index, Integer_literal n);
      Let_list_access(value, listname, index);
    ]
  in
  directives, value
;;

let extract_arg_to_value ctx annot arglist n =
  let list_extract, arg_loc = extract_nth_list_elt ctx annot arglist n in
  let get_obj, arg_obj = get_value ctx annot arg_loc in
  let extract_val_loc, val_loc = get_attr ctx annot "*value" arg_obj in
  let get_val, arg_val = get_value ctx annot val_loc in
  let all_extractions =
    List.map (annotate_directive ctx annot) list_extract @
    get_obj @
    extract_val_loc @
    get_val
  in
  all_extractions, arg_val
;;
