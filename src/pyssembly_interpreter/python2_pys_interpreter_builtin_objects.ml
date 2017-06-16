open Python2_pys_interpreter_types;;

let make_int_obj (m : memloc) : Bindings.t =
  let obj = Bindings.empty
            |> Bindings.update_binding "*value" m
  in
  obj
;;

let make_float_obj (m : memloc) : Bindings.t =
  let obj = Bindings.empty
            |> Bindings.update_binding "*value" m
  in
  obj
;;

let make_string_obj (m : memloc) : Bindings.t =
  let obj = Bindings.empty
            |> Bindings.update_binding "*value" m
  in
  obj
;;

let make_bool_obj (m : memloc) : Bindings.t =
  let obj = Bindings.empty
            |> Bindings.update_binding "*value" m
  in
  obj
;;

let make_none_obj (m : memloc) : Bindings.t =
  let obj = Bindings.empty
            |> Bindings.update_binding "*value" m
  in
  obj
;;

let make_function_obj (m : memloc) : Bindings.t =
  let obj = Bindings.empty
            |> Bindings.update_binding "*value" m
  in
  obj
;;

let make_method_obj (m : memloc) : Bindings.t =
  let obj = Bindings.empty
            |> Bindings.update_binding "*value" m
  in
  obj
;;

let make_list_obj (m : memloc) : Bindings.t =
  let obj = Bindings.empty
            |> Bindings.update_binding "*value" m
  in
  obj
;;

let make_tuple_obj (m : memloc) : Bindings.t =
  let obj = Bindings.empty
            |> Bindings.update_binding "*value" m
  in
  obj
;;
