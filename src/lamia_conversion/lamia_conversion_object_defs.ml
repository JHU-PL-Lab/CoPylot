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
  let%bind obj_with_value = fresh_value_var () in

  let%bind _ = emit
      [
        Let_alloc(value_loc);
        Store(value_loc, x);
        Let_alloc(obj_loc);
        Let_expression(empty_obj, Empty_binding);
        Let_expression(value_name, String_literal("*value"));
        Let_binding_update(obj_with_value, empty_obj, value_name, value_loc);
      ]
  in
  let%bind filled_obj = fill_func obj_with_value in
  let%bind _ = emit
      [
        Store(obj_loc, filled_obj);
      ]
  in
  return obj_loc
;;

let fill_int obj =
  (* This should be a method, but because we don't have classes we store it
     as a function value directly *)
  let%bind obj = add_binding obj "__add__" int_add in
  (* TODO: More of this *)
  return obj
;;

let fill_float obj =
  (* TODO: Implement this *)
  return obj
;;

let fill_bool obj =
  (* TODO: Implement this *)
  return obj
;;

let fill_string obj =
  (* TODO: Implement this *)
  return obj
;;

let fill_list obj =
  (* TODO: Implement this *)
  return obj
;;

let fill_tuple obj =
  (* TODO: Implement this *)
  return obj
;;

let fill_func obj =
  (* TODO: Implement this *)
  return obj
;;

(* Methods will have as their *value a function value, which will call their
   __func__ attribute with __self__ as the first argument *)
let fill_method obj_loc obj =
  ignore obj_loc; (* TODO: This should be bound to __self__ *)
  (* TODO: Implement this *)
  return obj
;;

let fill_name_error obj =
  (* TODO: Implement this *)
  return obj
;;

let fill_attribute_error obj =
  (* TODO: Implement this *)
  return obj
;;

let fill_type_error obj =
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

(* Wrapping methods is significantly more complicated than other wraps. *)
let wrap_method func self =
  (* A lamia function value which curries in self automatically *)
  let%bind curried_func =
    let%bind new_func_name = fresh_value_var () in
    let%bind arglist = fresh_value_var () in
    let%bind _, new_func_body =
      listen @@
      let%bind self_list = fresh_value_var () in
      let%bind curried_args = fresh_value_var () in
      let%bind retval = fresh_memory_var () in
      emit
        [
          Let_expression(self_list, List_value [self]);
          Let_binop(curried_args, self_list, Binop_listconcat, arglist);
          Let_call_function(retval, func, [curried_args]);
        ]
    in
    let%bind _ = emit
        [
          Let_expression(new_func_name,
                         Function_expression([arglist], Block new_func_body));
        ]
    in
    return new_func_name
  in
  wrap_obj (fill_method self) curried_func;;
