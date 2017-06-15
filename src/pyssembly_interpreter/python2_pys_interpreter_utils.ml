(* open Batteries;; *)
open Python2_ast_types;;
open Python2_normalized_ast;;
open Python2_pys_interpreter_types;;

let bind_var
    (env : Environment.t)
    (eta : eta)
    (var : identifier)
    (m : memloc)
  : Environment.t =
  let bindings = Environment.get_binding env eta in
  let new_bindings = Bindings.update_binding bindings var m in
  let new_env = Environment.update_binding env eta new_bindings in
  new_env
;;

let allocate_memory (h : Heap.t) (v : value) : Heap.t * memloc =
  let m = Heap.get_new_memloc h in
  let new_heap = Heap.update_binding h m v in
  new_heap, m
;;

let literal_to_value (l : literal) (eta : eta): value =
  let module Normalized = Python2_normalized_ast in
  match l with
  | Normalized.Num (n)     -> Num(n)
  | Normalized.Str (s)     -> Str(s)
  | Normalized.Bool (b)    -> Bool(b)
  | Normalized.Builtin (b) -> Builtin(b)
  | Normalized.NoneVal     -> NoneVal
  | Normalized.FunctionVal (args, body)
    -> Function (eta, args, Body.create body)
;;

let simple_advance_stack (frame : Stack_frame.t) (stack : Program_stack.t)
  : Program_stack.t =
  let next_label = Stack_frame.get_next_label frame in
  let next_frame = Stack_frame.advance frame next_label in
  let next_stack = Program_stack.push stack next_frame in
  next_stack
;;

let rec lookup
    (id: identifier)
    (env : Environment.t)
    (parents : Parents.t)
    (eta : eta)
  : memloc option =
  let bindings = Environment.get_binding env eta in
  let m = Bindings.get_memloc bindings id in
  match m with
  | Some _ -> m
  | None ->
    let parent = Parents.get_parent parents eta in
    match parent with
    | None -> None
    | Some(p) -> lookup id env parents p
;;
