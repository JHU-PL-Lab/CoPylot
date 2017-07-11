open Batteries;;
open Lamia_ast;;
open Lamia_conversion_ctx;;

(* Name of our local python scope variable *)
let python_scope = Memory_variable("scope");;

let map_and_concat (func : 'a -> 'b list) (lst : 'a list) =
  List.concat (List.map func lst)
;;

let annotate_directive ctx annot d =
  Statement(get_next_uid ctx annot, d)
;;

let lookup ctx annot id =
  let scopeval = Value_variable(gen_unique_name ctx annot) in
  let varname = Value_variable(gen_unique_name ctx annot) in
  let haskey = Value_variable(gen_unique_name ctx annot) in
  let retval = Memory_variable(gen_unique_name ctx annot) in

  let success =
    let ret_memloc = Memory_variable(gen_unique_name ctx annot) in
    [
      Let_binding_access(ret_memloc, scopeval, varname);
      If_result_memory(ret_memloc);
    ]
  in
  let fail =
    let error_memloc = Memory_variable(gen_unique_name ctx annot) in
    [
      (* TODO: Alloc NameError *)
      Raise error_memloc;
    ]
  in
  let success_block =
    Block (List.map (annotate_directive ctx annot) success)
  in
  let fail_block =
    Block (List.map (annotate_directive ctx annot) fail)
  in

  List.map (annotate_directive ctx annot)
    [
      Let_get(scopeval, python_scope);
      Let_expression(varname, String_literal(id));
      Let_binop(haskey, scopeval, Binop_haskey, varname);
      Let_conditional_memory(retval, haskey, success_block, fail_block);
    ],
  retval
;;

let get_starvalue ctx annot y =
  let obj = Value_variable(gen_unique_name ctx annot) in
  let valuename = Value_variable(gen_unique_name ctx annot) in
  let starvalue = Memory_variable(gen_unique_name ctx annot) in
  let directives =
    [
      Let_get(obj, y);
      Let_expression(valuename, String_literal("*value"));
      Let_binding_access(starvalue, obj, valuename);
    ]
  in
  List.map (annotate_directive ctx annot) directives,
  starvalue
;;
