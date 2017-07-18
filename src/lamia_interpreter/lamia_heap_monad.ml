open Lamia_evaluation_ast;;
open Lamia_evaluation_grammar;;
open Lamia_heap;;

type heap_state =
  { heap : Heap.t;
    next_address : int;
  }
;;

type 'a heap_result =
  | Heap_success of 'a * heap_state
  | Heap_error of string * heap_state
;;

type 'a m = heap_state -> 'a heap_result;;
let return (x : 'a) : 'a m = fun heap_state -> Heap_success(x,heap_state);;
let bind (x : 'a m) (f : 'a -> 'b m) : 'b m =
  fun heap_state ->
    match x heap_state with
    | Heap_success(v1,heap_state') -> f v1 heap_state'
    | Heap_error(s,heap_state') -> Heap_error(s,heap_state')
;;

let sequence (xs : 'a m list) : 'a list m =
  let rec loop (acc : 'a list) (xs' : 'a m list) : 'a list m =
    match xs' with
    | [] -> return acc
    | x::xs'' ->
      let%bind z = x in
      loop (z::acc) xs''
  in
  let%bind zs = loop [] xs in
  return @@ List.rev zs
;;

let get_value x : value m =
  fun heap_state ->
    try
      Heap_success(
        Heap.get_value x heap_state.heap,
        heap_state
      )
    with
    | Not_found ->
      Heap_error(
        Printf.sprintf "Attempted to look up unbound value variable %s"
          (show_value_variable x),
        heap_state)
;;

let get_memory_address y : memory_address m =
  fun heap_state ->
    try
      Heap_success(
        Heap.get_memory_address y heap_state.heap,
        heap_state
      )
    with
    | Not_found ->
      Heap_error(
        Printf.sprintf "Attempted to look up unbound memory variable %s"
          (show_memory_variable y),
        heap_state)
;;

let get_heap_value m : value m =
  fun heap_state ->
    try
      Heap_success(
        Heap.get_heap_value m heap_state.heap,
        heap_state
      )
    with
    | Not_found ->
      Heap_error(
        Printf.sprintf "Attempted to look up unbound memory address %s"
          (show_memory_address m),
        heap_state)
;;

let set_value x v : unit m =
  fun heap_state ->
    Heap_success(
      (),
      { heap_state with heap = Heap.set_value x v heap_state.heap }
    )
;;

let set_memory_address y m : unit m =
  fun heap_state ->
    Heap_success(
      (),
      { heap_state with heap = Heap.set_memory_address y m heap_state.heap }
    )
;;

let set_heap_value m v : unit m =
  fun heap_state ->
    Heap_success(
      (),
      { heap_state with heap = Heap.set_heap_value m v heap_state.heap }
    )
;;

let fresh_address () : memory_address m =
  fun heap_state ->
    Heap_success(
      Memory_address(heap_state.next_address),
      { heap_state with next_address = heap_state.next_address + 1 }
    )
;;

let force_error s : 'a m =
  fun heap ->
    Heap_error(s,heap)
;;

let run (x : 'a m)  : 'a heap_result =
  let initial_state =
    { heap = Heap.empty;
      next_address = 0;
    }
  in
  x initial_state
;;
