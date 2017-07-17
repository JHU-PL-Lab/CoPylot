open Lamia_evaluation_ast;;
open Lamia_evaluation_grammar;;

module Heap :
sig
  type t
  val empty : t
  val get_value : value_variable -> t -> value;;
  val get_memory_address : memory_variable -> t -> memory_address;;
  val get_heap_value : memory_address -> t -> value;;
  val set_value : value_variable -> value -> t -> t;;
  val set_memory_address : memory_variable -> memory_address -> t -> t;;
  val set_heap_value : memory_address -> value -> t -> t;;
  val to_string : t -> string
end;;
