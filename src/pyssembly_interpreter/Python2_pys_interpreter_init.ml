open Python2_pys_interpreter_types;;

(* Here we define what the starting program state looks like. The only one
   that is nontrivial at the moment is the starting heap *)
let global_memloc = Memloc(0);;

let starting_bindings =
  Bindings.empty
  |> Bindings.update_binding "*None" None_memloc
  |> Bindings.update_binding "NameError" @@ Builtin_exn_memloc(Builtin_NameError)
  |> Bindings.update_binding "TypeError" @@ Builtin_exn_memloc(Builtin_TypeError)
  |> Bindings.update_binding "ValueError" @@ Builtin_exn_memloc(Builtin_ValueError)
  |> Bindings.update_binding "AttributeError" @@ Builtin_exn_memloc(Builtin_AttributeError)
;;

(* Handy regex for adding more builtin functions:
   copy in the definition of the builtin_function type, then replace
   "^( *)\| (.+)( *)$"
   with
   |> Heap.update_binding (Builtin_fun_memloc($2)) (Function(Builtin_func($2)))
*)
let starting_heap =
  Heap.empty
  |> Heap.update_binding global_memloc @@ Bindings(starting_bindings)
  (* Globals *)
  |> Heap.update_binding (Builtin_fun_memloc(Builtin_bool)) (Function(Builtin_func(Builtin_bool)))
  |> Heap.update_binding (Builtin_fun_memloc(Builtin_slice)) (Function(Builtin_func(Builtin_slice)))
  |> Heap.update_binding (Builtin_fun_memloc(Builtin_type_func)) (Function(Builtin_func(Builtin_type_func)))
  |> Heap.update_binding (Builtin_fun_memloc(Builtin_call)) (Function(Builtin_func(Builtin_call)))
  (* Methods *)
  (* --Ints-- *)
  |> Heap.update_binding (Builtin_fun_memloc(Builtin_int_add)) (Function(Builtin_func(Builtin_int_add)))
  |> Heap.update_binding (Builtin_fun_memloc(Builtin_int_neg)) (Function(Builtin_func(Builtin_int_neg)))
  (* --Floats-- *)
  |> Heap.update_binding (Builtin_fun_memloc(Builtin_float_add)) (Function(Builtin_func(Builtin_float_add)))
  |> Heap.update_binding (Builtin_fun_memloc(Builtin_float_neg)) (Function(Builtin_func(Builtin_float_neg)))
  (* --Strings-- *)
  |> Heap.update_binding (Builtin_fun_memloc(Builtin_string_add)) (Function(Builtin_func(Builtin_string_add)))
  |> Heap.update_binding (Builtin_fun_memloc(Builtin_string_contains)) (Function(Builtin_func(Builtin_string_contains)))
  (* --Lists-- *)
  |> Heap.update_binding (Builtin_fun_memloc(Builtin_list_add)) (Function(Builtin_func(Builtin_list_add)))
  |> Heap.update_binding (Builtin_fun_memloc(Builtin_list_iter)) (Function(Builtin_func(Builtin_list_iter)))
  |> Heap.update_binding (Builtin_fun_memloc(Builtin_list_contains)) (Function(Builtin_func(Builtin_list_contains)))
  |> Heap.update_binding (Builtin_fun_memloc(Builtin_list_getitem)) (Function(Builtin_func(Builtin_list_getitem)))
  (* --Tuples-- *)
  |> Heap.update_binding (Builtin_fun_memloc(Builtin_tuple_add)) (Function(Builtin_func(Builtin_tuple_add)))
  |> Heap.update_binding (Builtin_fun_memloc(Builtin_tuple_iter)) (Function(Builtin_func(Builtin_tuple_iter)))
  |> Heap.update_binding (Builtin_fun_memloc(Builtin_tuple_contains)) (Function(Builtin_func(Builtin_tuple_contains)))
  |> Heap.update_binding (Builtin_fun_memloc(Builtin_tuple_getitem)) (Function(Builtin_func(Builtin_tuple_getitem)))

;;
