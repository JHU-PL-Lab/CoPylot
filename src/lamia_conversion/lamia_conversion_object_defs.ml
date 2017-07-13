(* open Batteries;; *)
open Lamia_ast;;
open Lamia_conversion_builtin_names;;
open Lamia_conversion_monad;;
open Conversion_monad;;

let add_binding
    (obj : value_variable)
    (attr : string)
    (value : memory_variable)
  : value_variable m =
  let%bind attr_val = fresh_value_var () in
  let%bind new_obj = fresh_value_var () in
  let%bind _ = emit
      [
        Let_expression(attr_val, String_literal attr);
        Let_binding_update(new_obj, obj, attr_val, value);
      ]
  in
  return new_obj
;;

let wrap_obj
    fill_func
    (x : value_variable)
  : memory_variable m =
  let%bind value_loc = fresh_memory_var () in
  let%bind obj_loc = fresh_memory_var () in
  let%bind value_name = fresh_value_var () in
  let%bind empty_obj = fresh_value_var () in
  let%bind obj_val2 = fresh_value_var () in

  let%bind _ = emit
      [
        Let_alloc(value_loc);
        Store(value_loc, x);
        Let_alloc(obj_loc);
        Let_expression(empty_obj, Empty_binding);
        Let_expression(value_name, String_literal("*value"));
        Let_binding_update(obj_val2, empty_obj, value_name, value_loc);
      ]
  in
  let%bind filled_obj = fill_func obj_val2 obj_loc in
  let%bind _ = emit
      [
        Store(obj_loc, filled_obj);
      ]
  in
  return obj_loc
;;

let fill_int obj obj_loc =
  ignore obj_loc;
  (* FIXME: This needs to be a method, not a function *)
  let%bind obj = add_binding obj "__add__" int_add in
  (* TODO: More of this *)
  return obj
;;

let fill_float obj _ =
  (* TODO: Implement this *)
  return obj
;;

let fill_bool obj _ =
  (* TODO: Implement this *)
  return obj
;;

let fill_string obj _ =
  (* TODO: Implement this *)
  return obj
;;

let fill_list obj _ =
  (* TODO: Implement this *)
  return obj
;;

let fill_tuple obj _ =
  (* TODO: Implement this *)
  return obj
;;

let fill_func obj _ =
  (* TODO: Implement this *)
  return obj
;;

let fill_name_error obj _ =
  (* TODO: Implement this *)
  return obj
;;

let fill_attribute_error obj _ =
  (* TODO: Implement this *)
  return obj
;;

let fill_type_error obj _ =
  (* TODO: Implement this *)
  return obj
;;

(* The "right" way to handle all these similar functions is to make a type
   representing what type the object is, and pass that in as a parameter.
   However, a lot of this process will change once we implement classes,
   so I'm decreeing this to be acceptable for the moment. *)
let wrap_int x = wrap_obj fill_int x;;
let wrap_float x = wrap_obj fill_float x;;
let wrap_bool x = wrap_obj fill_bool x;;
let wrap_string x = wrap_obj fill_string x;;
let wrap_list x = wrap_obj fill_list x;;
let wrap_tuple x = wrap_obj fill_tuple x;;
let wrap_func x = wrap_obj fill_func x;;
(* We're going to pretend that exceptions are just regular objects, whose
   *value field is a string *)
let wrap_name_error = wrap_obj fill_name_error;;
let wrap_attribute_error = wrap_obj fill_attribute_error;;
let wrap_type_error = wrap_obj fill_type_error;;
