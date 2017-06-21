(* open Batteries;; *)
(* open Python2_ast_types;; *)
open Python2_normalized_ast;;
open Python2_pys_interpreter_types;;
(* open Python2_pys_interpreter_builtin_objects;; *)
(* open Jhupllib_utils;; *)

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

let pop_var_or_fail
    (micro: Micro_instruction_stack.t)
    (failmsg: string)
  : identifier * Micro_instruction_stack.t =
  let inert, popped_stack = Micro_instruction_stack.pop_first_inert micro in
  match inert with
  | Micro_var(v) -> v, popped_stack
  | _ -> failwith failmsg
;;

let pop_memloc_or_fail
    (micro: Micro_instruction_stack.t)
    (failmsg: string)
  : memloc * Micro_instruction_stack.t =
  let inert, popped_stack = Micro_instruction_stack.pop_first_inert micro in
  match inert with
  | Micro_memloc(m) -> m, popped_stack
  | _ -> failwith failmsg
;;

let pop_value_or_fail
    (micro: Micro_instruction_stack.t)
    (failmsg: string)
  : value * Micro_instruction_stack.t =
  let inert, popped_stack = Micro_instruction_stack.pop_first_inert micro in
  match inert with
  | Micro_value(v) -> v, popped_stack
  | _ -> failwith failmsg
;;

(* let make_literal_obj (m : memloc) (l : literal) : Bindings.t =
  let module Normalized = Python2_normalized_ast in
  match l with
  | Normalized.Num (Int _)   ->   make_int_obj m
  | Normalized.Num (Float _) ->   make_float_obj m
  | Normalized.Str _         ->   make_string_obj m
  | Normalized.Bool _        ->   make_bool_obj m
  | Normalized.FunctionVal _ ->   make_function_obj m
  | Normalized.NoneVal       ->   make_none_obj m
  | Normalized.Builtin _     ->   failwith "NYI: Builtin object"

;; *)

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

let get_active_or_fail (frame : Stack_frame.t) : annotated_stmt =
  let active = Stack_frame.active_stmt frame in
  extract_option_or_fail active "Failed to find active statement"
;;

let get_parent_or_fail (child : memloc) (parents : Parents.t) : memloc =
  let parent = Parents.get_parent child parents in
  extract_option_or_fail parent "Failed to find parent scope"
;;

let rec lookup
    (eta : memloc)
    (parents : Parents.t)
    (heap : Heap.t)
    (id: identifier)
  : memloc option =
  let bindings = retrieve_binding_or_fail heap eta in
  let m = Bindings.get_memloc id bindings in
  match m with
  | Some _ -> m
  | None ->
    let parent = Parents.get_parent eta parents in
    match parent with
    | None -> None
    | Some(p) -> lookup p parents heap id
;;

(* let get_obj_value (heap : Heap.t) (m : memloc) (attr : identifier)
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
;; *)

(* Warning: This can loop infinitely if the code does something like
   x.__call__ = x *)
(* let rec get_call (heap : Heap.t) (m: memloc) : memloc option =
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
;; *)
