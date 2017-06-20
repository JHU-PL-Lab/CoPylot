(* open Batteries;; *)
open Python2_ast_types;;
open Python2_normalized_ast;;
open Python2_pys_interpreter_types;;
open Python2_pys_interpreter_builtin_objects;;
open Jhupllib_utils;;

let make_literal_obj (m : memloc) (l : literal) : Bindings.t =
  let module Normalized = Python2_normalized_ast in
  match l with
  | Normalized.Num (Int _)   ->   make_int_obj m
  | Normalized.Num (Float _) ->   make_float_obj m
  | Normalized.Str _         ->   make_string_obj m
  | Normalized.Bool _        ->   make_bool_obj m
  | Normalized.FunctionVal _ ->   make_function_obj m
  | Normalized.NoneVal       ->   make_none_obj m
  | Normalized.Builtin _     ->   make_function_obj m
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

let get_active_or_fail (frame : Stack_frame.t) : annotated_stmt =
  let active = Stack_frame.active_stmt frame in
  match active with
  | None -> failwith "Failed to find active statement"
  | Some(stmt) -> stmt
;;

let get_parent_or_fail (parents : Parents.t) (child : memloc) : memloc =
  let parent = Parents.get_parent child parents in
  match parent with
  | None -> failwith "Failed to find parent scope"
  | Some(p) -> p
;;

let bind_var
    (heap : Heap.t)
    (bindings_loc : memloc)
    (var : identifier)
    (target : memloc)
  : Heap.t =
  let bindings = retrieve_binding_or_fail heap bindings_loc in
  let new_bindings = Bindings.update_binding var target bindings in
  let new_env = Heap.update_binding bindings_loc (Bindings(new_bindings)) heap in
  new_env
;;

let allocate_memory (h : Heap.t) (v : value) : Heap.t * memloc =
  let m = Heap.get_new_memloc h in
  let new_heap = Heap.update_binding m v h in
  new_heap, m
;;

let convert_builtin b =
  let module Ast = Python2_ast_types in
  match b with
  | Ast.Builtin_bool -> Function(Builtin_func(Builtin_bool))
  | Ast.Builtin_slice -> Function(Builtin_func(Builtin_slice))
  | Ast.Builtin_type -> Function(Builtin_func(Builtin_type))
  | Ast.Builtin_AttributeError -> Builtin_exception(Builtin_AttributeError)
  | Ast.Builtin_ValueError -> Builtin_exception(Builtin_ValueError)
;;

let literal_to_value (l : literal) (curr_m : memloc): value =
  let module Normalized = Python2_normalized_ast in
  match l with
  | Normalized.Num (n)     -> Num(n)
  | Normalized.Str (s)     -> Str(s)
  | Normalized.Bool (b)    -> Bool(b)
  | Normalized.Builtin (b) -> convert_builtin b
  | Normalized.NoneVal     -> NoneVal
  | Normalized.FunctionVal (args, body)
    -> Function (User_func(curr_m, args, Body.create body))
;;

let simple_advance_stack (frame : Stack_frame.t) (stack : Program_stack.t)
  : Program_stack.t =
  let next_label = Stack_frame.get_next_label frame in
  let next_frame = Stack_frame.advance frame next_label in
  let next_stack = Program_stack.push stack next_frame in
  next_stack
;;

let rec lookup
    (heap : Heap.t)
    (parents : Parents.t)
    (bindings_loc : memloc)
    (id: identifier)
  : memloc option =
  let bindings = retrieve_binding_or_fail heap bindings_loc in
  let m = Bindings.get_memloc id bindings in
  match m with
  | Some _ -> m
  | None ->
    let parent = Parents.get_parent bindings_loc parents in
    match parent with
    | None -> None
    | Some(p) -> lookup heap parents p id
;;

let lookup_or_error (* TODO: Remove this function *)
    (heap : Heap.t)
    (parents : Parents.t)
    (bindings_loc : memloc)
    (id: identifier)
  : memloc =
  let memloc = lookup heap parents bindings_loc id in
  match memloc with
  | None -> raise @@ Not_yet_implemented "NYI: Throw NameError"
  | Some(m) -> m
;;

let get_obj_attribute (heap : Heap.t) (m : memloc) (attr : identifier)
  : value option =
  let obj = Heap.get_value m heap in
  match obj with
  | Bindings(bindings) ->
    let value_memloc = Bindings.get_memloc "*value" bindings in
    begin
      match value_memloc with
      | None -> None
      | Some(memloc) ->
        let actual_value = Heap.get_value memloc heap in
        Some(actual_value)
    end
  | _ -> failwith "Asked to find *value of non-object"
;;

(* Given a program and a location where an exception is stored, pop stack frames
   until you see a catch statement, and bind the given memory location to the
   specified variable. *)
let rec throw_exception prog memloc =
  let stack_top, stack_body = Program_stack.pop prog.stack in
  let active = get_active_or_fail stack_top in
  match active.exception_target with
  | None -> (* No exception target *)
    let prev_m = get_parent_or_fail prog.parents prog.m in
    (* Base case: empty stack, nothing more to pop *)
    if Program_stack.is_empty stack_body then
      { stack = stack_body; heap = prog.heap; parents = prog.parents; m = prog.m }
    else
      (* No catch here, go to the next stack frame *)
      throw_exception
        { stack = stack_body; heap = prog.heap; parents = prog.parents; m = prev_m }
        memloc

  | Some(target) -> (* Hopefully target points to a catch *)
    begin
      match Stack_frame.get_stmt stack_top target with
      | Some({body = Catch(id); _}) ->
        (* Bind the given exception to id *)
        let new_heap = bind_var prog.heap prog.m id memloc in
        (* Update stack_top so that it's active stmt is the catch we just ran *)
        let updated_frame = Stack_frame.advance stack_top @@ Uid(target) in
        (* Move the active stmt pointer once more, past the catch *)
        let new_stack = simple_advance_stack updated_frame stack_body  in
        { stack = new_stack; heap = new_heap; parents = prog.parents; m = prog.m }
      | _ -> failwith "Exception target did not point to catch in scope"
    end
;;

(* Warning: This can loop infinitely if the code does something like
   x.__call__ = x *)
let rec get_call (heap : Heap.t) (m: memloc) : memloc option =
  let obj = Heap.get_value m heap in
  match obj with
  | Bindings(bindings) ->
    let value_memloc = Bindings.get_memloc "__call__" bindings in
    begin
      match value_memloc with
      | None -> None (* Not callable *)
      | Some(memloc) ->
        let actual_value = Heap.get_value memloc heap in
        match actual_value with
        | Bindings(b) -> get_call heap memloc
        | Function _
        | Method _ -> Some(memloc)
        | _ -> None (* Not callable *)

    end
  | _ -> None (* Not callable *)
;;
