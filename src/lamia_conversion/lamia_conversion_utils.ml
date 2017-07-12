open Batteries;;
open Lamia_ast;;
open Lamia_conversion_builtin_names;;
open Lamia_conversion_ctx;;

let map_and_concat (func : 'a -> 'b list) (lst : 'a list) =
  List.concat (List.map func lst)
;;

let annotate_directive ctx annot d =
  Statement(get_next_uid ctx annot, d)
;;

let store_value ctx annot
    (v : value_expression) =
  let value_name = Value_variable(gen_unique_name ctx annot) in
  [
    annotate_directive ctx annot @@
    Let_expression(value_name, v);
  ],
  value_name
;;

(* Get the memloc with key id from the binding at binding_loc. If no such
   memloc exists, run the directives in on_failure. *)
let get_from_binding ctx annot target binding_loc on_failure =
  let bindingval = Value_variable(gen_unique_name ctx annot) in
  let haskey = Value_variable(gen_unique_name ctx annot) in
  let retval = Memory_variable(gen_unique_name ctx annot) in

  let success =
    let ret_memloc = Memory_variable(gen_unique_name ctx annot) in
    [
      Let_binding_access(ret_memloc, bindingval, target);
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
      Let_get(bindingval, binding_loc);
      Let_binop(haskey, bindingval, Binop_haskey, target);
      Let_conditional_memory(retval, haskey, success_block, fail_block);
    ],
  retval
;;

let lookup ctx annot (id : string) =
  ignore ctx; ignore annot; ignore id; failwith ""
;;

let get_attr ctx annot target bindings_loc =
  let exn_loc = Memory_variable(gen_unique_name ctx annot) in
  let alloc_exn =
    ignore exn_loc;
    [
      (* TODO: Alloc & throw AttributeError *)
    ]
  in
  get_from_binding ctx annot target bindings_loc alloc_exn
;;

let lookup_and_get_attr ctx annot varname attr =
  let lookup_bindings, lookup_result = lookup ctx annot varname in
  let target_bindings, target_result =
    store_value ctx annot @@ String_literal(attr)
  in
  let attr_bindings, attr_result =
    get_attr ctx annot target_result lookup_result
  in
  lookup_bindings @ target_bindings @ attr_bindings,
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

let convert_list convert_func lst =
  let converted_list = List.map convert_func lst in
  let extract
      (tup1 : 'a list * 'b )
      (tup2 : 'a list * 'b list)
    : 'a list * 'b list =
    (fst tup1 @ fst tup2, (snd tup1)::(snd tup2)) in
  let bindings, results =
    List.fold_right extract converted_list ([], []) in
  bindings, results
;;
