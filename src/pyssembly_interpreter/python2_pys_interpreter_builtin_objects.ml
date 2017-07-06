open Python2_ast_types;;
open Python2_pys_interpreter_types;;
open Jhupllib_utils;;

let get_fill_commands (m : memloc) (v: value) : micro_instruction list =
  (* Helper functions *)
  let store_method met =
    [
      Command(ALLOC);
      Inert(Micro_value(Method(m, Builtin_func(met))));
      Command(STORE);
    ]
  in
  let store_type t =
    [Inert(Micro_memloc(Builtin_type_memloc(t)))]
  in
  let bind_all lst =
    List.concat @@ List.map
      (fun (var, micro) ->
         [Inert(Micro_memloc m)] @ micro @ [Inert(Micro_var(var)); Command(BIND)])
      lst
  in
  let type_bindings =
    match v with
    | Bool _
    | Num(Int _) ->
      [
        ("__class__", store_type Int_type);
        ("__add__", store_method Builtin_int_add);
        ("__neg__", store_method Builtin_int_neg);
      ]
    | Num(Float _) ->
      [
        ("__class__", store_type Float_type);
        ("__add__", store_method Builtin_float_add);
        ("__neg__", store_method Builtin_float_neg);
      ]
    | Str _ ->
      [
        ("__class__", store_type String_type);
        ("__add__", store_method Builtin_string_add);
        ("__contains__", store_method Builtin_string_contains);
      ]
    | ListVal _ ->
      [
        ("__class__", store_type List_type);
        ("__add__", store_method Builtin_list_add);
        ("__contains__", store_method Builtin_list_contains);
        ("__getitem__", store_method Builtin_list_getitem);
      ]
    | TupleVal _ ->
      [
        ("__class__", store_type Tuple_type);
        ("__add__", store_method Builtin_tuple_add);
        ("__contains__", store_method Builtin_tuple_contains);
        ("__getitem__", store_method Builtin_tuple_getitem);
      ]
    | Function _ ->
      [
        ("__class__", store_type Function_type);
      ]
    | Method _ ->
      [
        ("__class__", store_type Method_wrapper_type);
        ("__add__", store_method Builtin_call);
      ]
    | Builtin_type _ ->
      [
        (* For the moment we never look at the object, so this is empty *)
      ]
    | Builtin_exception _
    | NoneVal -> raise @@ Not_yet_implemented "Wrapping value"
    | Bindings _ -> failwith "Tried to fill existing bindings"
  in
  bind_all type_bindings
;;
