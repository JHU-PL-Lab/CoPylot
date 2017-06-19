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

let retrieve_binding (heap : Heap.t) (m : memloc) : Bindings.t =
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
  let bindings = retrieve_binding heap bindings_loc in
  let new_bindings = Bindings.update_binding var target bindings in
  let new_env = Heap.update_binding bindings_loc (Bindings(new_bindings)) heap in
  new_env
;;

let allocate_memory (h : Heap.t) (v : value) : Heap.t * memloc =
  let m = Heap.get_new_memloc h in
  let new_heap = Heap.update_binding m v h in
  new_heap, m
;;

let literal_to_value (l : literal) (curr_m : memloc): value =
  let module Normalized = Python2_normalized_ast in
  match l with
  | Normalized.Num (n)     -> Num(n)
  | Normalized.Str (s)     -> Str(s)
  | Normalized.Bool (b)    -> Bool(b)
  | Normalized.Builtin (b) -> Builtin(b)
  | Normalized.NoneVal     -> NoneVal
  | Normalized.FunctionVal (args, body)
    -> Function (curr_m, args, Body.create body)
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
  let bindings = retrieve_binding heap bindings_loc in
  let m = Bindings.get_memloc id bindings in
  match m with
  | Some _ -> m
  | None ->
    let parent = Parents.get_parent bindings_loc parents in
    match parent with
    | None -> None
    | Some(p) -> lookup heap parents p id
;;

let lookup_or_error
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

let get_obj_value (heap : Heap.t) (m : memloc) : value option =
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
