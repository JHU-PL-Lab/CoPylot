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

type 'a m;;
val return : 'a -> 'a m;;
val bind : 'a m -> ('a -> 'b m) -> 'b m;;

val sequence : 'a m list -> 'a list m;;

val get_value : value_variable -> value m;;
val get_memory_address : memory_variable -> memory_address m;;
val get_heap_value : memory_address -> value m;;

val set_value : value_variable -> value -> unit m;;
val set_memory_address : memory_variable -> memory_address -> unit m;;
val set_heap_value : memory_address -> value -> unit m;;

val fresh_address : unit -> memory_address m;;
val force_error : string -> 'a m;;

val run : 'a m -> 'a heap_result;;
