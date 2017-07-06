(* open Batteries;; *)
open Python2_ast_types;;
open Python2_normalized_ast;;
open Python2_pys_interpreter_types;;
open Python2_pys_interpreter_builtin_objects;;

let convert_builtin b =
  let module Ast = Python2_ast_types in
  match b with
  | Ast.Builtin_bool -> Function(Builtin_func(Builtin_bool))
  | Ast.Builtin_slice -> Function(Builtin_func(Builtin_slice))
  | Ast.Builtin_type -> Function(Builtin_func(Builtin_type_func))
  | Ast.Builtin_method_wrapper_type -> Builtin_type(Method_wrapper_type);
  | Ast.Builtin_AttributeError -> Builtin_exception(Builtin_AttributeError)
  | Ast.Builtin_ValueError -> Builtin_exception(Builtin_ValueError)
  | Ast.Builtin_TypeError -> Builtin_exception(Builtin_TypeError)

;;

let literal_to_value (l : literal) (curr_m : memloc): value =
  let module Normalized = Python2_normalized_ast in
  match l with
  | Normalized.Num (n)     -> Num(n)
  | Normalized.Str (s)     -> Str(s)
  | Normalized.Bool (b)    -> Bool(b)
  | Normalized.Builtin (b) -> convert_builtin b
  | Normalized.FunctionVal (args, body)
    -> Function (User_func(curr_m, args, (List.hd body).uid))
;;

let pop_var_or_fail
    (micro: Micro_instruction_stack.t)
    (caller: string)
  : identifier * Micro_instruction_stack.t =
  let inert, popped_stack = Micro_instruction_stack.pop_first_inert micro in
  match inert with
  | Micro_var(v) -> v, popped_stack
  | Micro_memloc _ -> failwith @@ "Command " ^ caller ^ " expected a variable, but got a memloc!"
  | Micro_value _ -> failwith @@ "Command " ^ caller ^ " expected a variable, but got a value!"
;;

let pop_memloc_or_fail
    (micro: Micro_instruction_stack.t)
    (caller: string)
  : memloc * Micro_instruction_stack.t =
  let inert, popped_stack = Micro_instruction_stack.pop_first_inert micro in
  match inert with
  | Micro_var _ -> failwith @@ "Command " ^ caller ^ " expected a memloc, but got a variable!"
  | Micro_memloc(m) -> m, popped_stack
  | Micro_value _ -> failwith @@ "Command " ^ caller ^ " expected a memloc, but got a value!"

;;

let pop_value_or_fail
    (micro: Micro_instruction_stack.t)
    (caller: string)
  : value * Micro_instruction_stack.t =
  let inert, popped_stack = Micro_instruction_stack.pop_first_inert micro in
  match inert with
  | Micro_value(v) -> v, popped_stack
  | Micro_var _ -> failwith @@ "Command " ^ caller ^ " expected a value, but got a variable!"
  | Micro_memloc _ -> failwith @@ "Command " ^ caller ^ " expected a value, but got a memloc!"
;;

(* Pop n elements from mstack, all of which must be memlocs. Return a list of
   the popped memlocs with the most recently popped ones first, and the mstack
   with the elements popped off*)
let rec pop_n_memlocs
    (n : int)
    (lst : memloc list)
    (mstack : Micro_instruction_stack.t)
    (caller: string)
  : memloc list * Micro_instruction_stack.t =
  if n = 0
  then
    lst, mstack
  else
    let next_memloc, next_mstack =
      pop_memloc_or_fail mstack @@ caller ^ " argument was not a memloc!"
    in
    pop_n_memlocs (n-1) (next_memloc::lst) next_mstack caller
;;

(* This is the implementation of GetObj in the spec *)
let wrap_value (m1 : memloc) (m2 : memloc) (v : value)
  : Micro_instruction_stack.t =
  let module MIS = Micro_instruction_stack in
  match v with
  | Bindings b -> MIS.create @@
    [Inert(Micro_memloc(m2)); Inert(Micro_value(Bindings(b)));]
  | _ ->
    let base_obj =
      Bindings.empty
      |> Bindings.update_binding "*value" m2
      |> Bindings.update_binding "__getattribute__" @@ Builtin_fun_memloc(Builtin_get_attribute)
    in
    let fill_commands = get_fill_commands m1 v in
    MIS.create @@
    [Inert(Micro_memloc(m1)); Inert(Micro_value(Bindings(base_obj))); Command(STORE)] @
    fill_commands @
    [Inert(Micro_memloc(m1)); Command(GET)]
;;

let extract_option_or_fail (opt: 'a option) (failmsg: string) : 'a =
  match opt with
  | None -> failwith failmsg
  | Some(v) -> v
;;

let retrieve_binding_or_fail (heap : Heap.t) (m : memloc) : Bindings.t =
  let hval = Heap.get_value m heap in
  let bindings =
    match hval with
    | Bindings(b) -> b
    | _ -> failwith "Failed to retrieve binding from heap"
  in
  bindings
;;

let get_active_or_fail (frame : Stack_frame.t) (ctx : program_context)
  : annotated_stmt =
  let active_uid = Stack_frame.get_active_uid frame in
  let active_stmt = Body.get_stmt ctx.program active_uid in
  extract_option_or_fail active_stmt "Failed to find active statement"

;;

let get_parent_or_fail (child : memloc) (heap : Heap.t) : memloc =
  let parent_binding = retrieve_binding_or_fail heap child in
  let parent = Bindings.get_memloc "*parent" parent_binding in
  extract_option_or_fail parent "Failed to find parent scope"
;;
