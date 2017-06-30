open Python2_pys_interpreter_types;;

let make_int_obj (m : memloc) : Bindings.t =
  let obj = Bindings.empty
            |> Bindings.update_binding "*value" m
            |> Bindings.update_binding "__class__" @@ Builtin_type_memloc(Int_type)
            |> Bindings.update_binding "__add__" @@ Builtin_fun_memloc(Builtin_int_add)
            |> Bindings.update_binding "__neg__" @@ Builtin_fun_memloc(Builtin_int_neg)
  in
  obj
;;

let make_float_obj (m : memloc) : Bindings.t =
  let obj = Bindings.empty
            |> Bindings.update_binding "*value" m
            |> Bindings.update_binding "__class__" @@ Builtin_type_memloc(Float_type)
            |> Bindings.update_binding "__add__" @@ Builtin_fun_memloc(Builtin_float_add)
            |> Bindings.update_binding "__neg__" @@ Builtin_fun_memloc(Builtin_float_neg)
  in
  obj
;;

let make_string_obj (m : memloc) : Bindings.t =
  let obj = Bindings.empty
            |> Bindings.update_binding "*value" m
            |> Bindings.update_binding "__class__" @@ Builtin_type_memloc(String_type)
            |> Bindings.update_binding "__add__" @@ Builtin_fun_memloc(Builtin_string_add)
            |> Bindings.update_binding "__contains__" @@ Builtin_fun_memloc(Builtin_string_contains)
  in
  obj
;;

let make_bool_obj (m : memloc) : Bindings.t =
  let obj = Bindings.empty
            |> Bindings.update_binding "*value" m
  in
  obj
;;

let make_none_obj (_ : memloc) : Bindings.t =
  failwith "Doesn't actually make sense to make a None object, since there can only be one"
  (* let obj = Bindings.empty
            |> Bindings.update_binding "*value" m
            |> Bindings.update_binding "__class__" @@ Builtin_type_memloc(None_type)
  in
  obj *)
;;

let make_function_obj (m : memloc) : Bindings.t =
  let obj = Bindings.empty
            |> Bindings.update_binding "*value" m
            |> Bindings.update_binding "__class__" @@ Builtin_type_memloc(Function_type)
  in
  obj
;;

let make_method_obj (m : memloc) : Bindings.t =
  let obj = Bindings.empty
            |> Bindings.update_binding "*value" m
            |> Bindings.update_binding "__class__" @@ Builtin_type_memloc(Method_wrapper_type)
  in
  obj
;;

let make_list_obj (m : memloc) : Bindings.t =
  let obj = Bindings.empty
            |> Bindings.update_binding "*value" m
            |> Bindings.update_binding "__class__" @@ Builtin_type_memloc(List_type)
            |> Bindings.update_binding "__add__" @@ Builtin_fun_memloc(Builtin_list_add)
            |> Bindings.update_binding "__iter__" @@ Builtin_fun_memloc(Builtin_list_iter)
            |> Bindings.update_binding "__contains__" @@ Builtin_fun_memloc(Builtin_list_contains)
            |> Bindings.update_binding "__getitem__" @@ Builtin_fun_memloc(Builtin_list_getitem)
  in
  obj
;;

let make_tuple_obj (m : memloc) : Bindings.t =
  let obj = Bindings.empty
            |> Bindings.update_binding "*value" m
            |> Bindings.update_binding "__class__" @@ Builtin_type_memloc(Tuple_type)
            |> Bindings.update_binding "__add__" @@ Builtin_fun_memloc(Builtin_tuple_add)
            |> Bindings.update_binding "__iter__" @@ Builtin_fun_memloc(Builtin_tuple_iter)
            |> Bindings.update_binding "__contains__" @@ Builtin_fun_memloc(Builtin_tuple_contains)
            |> Bindings.update_binding "__getitem__" @@ Builtin_fun_memloc(Builtin_tuple_getitem)
  in
  obj
;;
