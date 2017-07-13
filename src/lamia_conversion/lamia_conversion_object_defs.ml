(* open Batteries;; *)
open Lamia_ast;;
open Lamia_conversion_builtin_names;;
open Lamia_conversion_ctx;;
open Lamia_conversion_utils;;

let add_binding ctx annot
    (obj : value_variable)
    (attr : string)
    (value : memory_variable)
  : statement list * value_variable =
  let attr_val = Value_variable(gen_unique_name ctx annot) in
  let new_obj = Value_variable(gen_unique_name ctx annot) in

  let directives =
    [
      Let_expression(attr_val, String_literal attr);
      Let_binding_update(new_obj, obj, attr_val, value);
    ]
  in
  List.map (annotate_directive ctx annot) directives,
  new_obj
;;

let wrap_obj ctx annot
    fill_func
    (x : value_variable)
  : statement list * memory_variable =
  let value_loc = Memory_variable(gen_unique_name ctx annot) in
  let obj_loc = Memory_variable(gen_unique_name ctx annot) in
  let value_name = Value_variable(gen_unique_name ctx annot) in
  let empty_obj = Value_variable(gen_unique_name ctx annot) in
  let obj_val2 = Value_variable(gen_unique_name ctx annot) in

  let create_obj =
    List.map (annotate_directive ctx annot)
      [
        Let_alloc(value_loc);
        Store(value_loc, x);
        Let_alloc(obj_loc);
        Let_expression(empty_obj, Empty_binding);
        Let_expression(value_name, String_literal("*value"));
        Let_binding_update(obj_val2, empty_obj, value_name, value_loc);
      ]
  in
  let fill_obj, filled_obj = fill_func ctx annot obj_val2 obj_loc in
  let all_bindings =
    create_obj @
    fill_obj @
    [annotate_directive ctx annot @@ Store(obj_loc, filled_obj)]
  in
  all_bindings, obj_loc
;;

let fill_int ctx annot obj obj_loc =
  ignore obj_loc;
  (* FIXME: This needs to be a method, not a function *)
  let add, add_obj = add_binding ctx annot obj "__add__" int_add in
  (* TODO: More of this *)
  let all_bindings = add in
  all_bindings, add_obj
;;

let fill_float _ _ obj _ =
  (* TODO: Implement this *)
  [], obj
;;

let fill_bool _ _ obj _ =
  (* TODO: Implement this *)
  [], obj
;;

let fill_string _ _ obj _ =
  (* TODO: Implement this *)
  [], obj
;;

let fill_list _ _ obj _ =
  (* TODO: Implement this *)
  [], obj
;;

let fill_tuple _ _ obj _ =
  (* TODO: Implement this *)
  [], obj
;;

let fill_func _ _ obj _ =
  (* TODO: Implement this *)
  [], obj
;;

(* The "right" way to handle all these similar functions is to make a type
   representing what type the object is, and pass that in as a parameter.
   However, a lot of this process will change once we implement classes,
   so I'm decreeing this to be acceptable for the moment. *)
let wrap_int ctx annot x = wrap_obj ctx annot fill_int x;;
let wrap_float ctx annot x = wrap_obj ctx annot fill_float x;;
let wrap_bool ctx annot x = wrap_obj ctx annot fill_bool x;;
let wrap_string ctx annot x = wrap_obj ctx annot fill_string x;;
let wrap_list ctx annot x = wrap_obj ctx annot fill_list x;;
let wrap_tuple ctx annot x = wrap_obj ctx annot fill_tuple x;;
let wrap_func ctx annot x = wrap_obj ctx annot fill_func x;;

let wrap_exception ctx annot
    (* Until we implement classes, we'll say exceptions can only take one
       argument *)
    (x : value_variable)
  : statement list * memory_variable =
  (* TODO *)
  ignore ctx; ignore annot; ignore x;
  raise @@ Jhupllib_utils.Not_yet_implemented "wrap_exception"
;;
