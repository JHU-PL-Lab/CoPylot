(* open Batteries;; *)
open Lamia_ast;;
open Lamia_conversion_ctx;;
open Lamia_conversion_utils;;

let wrap_obj ctx annot
    (x : value_variable)
  : statement list * memory_variable =
  let value_loc = Memory_variable(gen_unique_name ctx annot) in
  let obj_loc = Memory_variable(gen_unique_name ctx annot) in
  let value_name = Value_variable(gen_unique_name ctx annot) in
  let obj_val1 = Value_variable(gen_unique_name ctx annot) in
  let obj_val2 = Value_variable(gen_unique_name ctx annot) in

  let directives =
    [
      Let_alloc(value_loc);
      Store(value_loc, x);
      Let_alloc(obj_loc);
      Let_expression(obj_val1, Empty_binding);
      Let_expression(value_name, String_literal("*value"));
      Let_binding_update(obj_val2, obj_val1, value_name, value_loc);
    ]
  in
  List.map (annotate_directive ctx annot) directives,
  obj_loc
;;

let wrap_int ctx annot
    (x : value_variable)
  : statement list * memory_variable =
  let obj_bindings, obj_loc = wrap_obj ctx annot x in
  (* TODO: Bind builtins *)
  obj_bindings, obj_loc
;;

let wrap_float ctx annot
    (x : value_variable)
  : statement list * memory_variable =
  let obj_bindings, obj_loc = wrap_obj ctx annot x in
  (* TODO: Bind builtins *)
  obj_bindings, obj_loc
;;

let wrap_bool ctx annot
    (x : value_variable)
  : statement list * memory_variable =
  let obj_bindings, obj_loc = wrap_obj ctx annot x in
  (* TODO: Bind builtins *)
  obj_bindings, obj_loc
;;

let wrap_string ctx annot
    (x : value_variable)
  : statement list * memory_variable =
  let obj_bindings, obj_loc = wrap_obj ctx annot x in
  (* TODO: Bind builtins *)
  obj_bindings, obj_loc
;;

let wrap_list ctx annot
    (x : value_variable)
  : statement list * memory_variable =
  let obj_bindings, obj_loc = wrap_obj ctx annot x in
  (* TODO: Bind builtins *)
  obj_bindings, obj_loc
;;

let wrap_tuple ctx annot
    (x : value_variable)
  : statement list * memory_variable =
  let obj_bindings, obj_loc = wrap_obj ctx annot x in
  (* TODO: Bind builtins *)
  obj_bindings, obj_loc
;;

let wrap_func ctx annot
    (x : value_variable)
  : statement list * memory_variable =
  let obj_bindings, obj_loc = wrap_obj ctx annot x in
  (* TODO: Bind builtins *)
  obj_bindings, obj_loc
;;
