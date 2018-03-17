open Batteries;;
open Jhupllib;;

open Lamia_evaluation_ast;;

module StringMap : Map.S with type key = string;;

type memory_address = Memory_address of int;;
val equal_memory_address : memory_address -> memory_address -> bool;;
val compare_memory_address : memory_address -> memory_address -> int;;
val pp_memory_address : memory_address Pp_utils.pretty_printer;;
val show_memory_address : memory_address -> string;;
module Memory_address :
sig
  type t = memory_address
  val equal : t -> t -> bool;;
  val compare : t -> t -> int;;
  val pp : t Pp_utils.pretty_printer;;
  val show : t -> string;;
end;;

type value =
  | Integer_value of int
  | String_value of string
  | Boolean_value of bool
  | List_value of memory_address list
  | Object_value of memory_address StringMap.t
  | Function_value of value_variable list * block
  | None_value
  | NotImplemented_value
;;
val equal_value : value -> value -> bool;;
val compare_value : value -> value -> int;;
val pp_value : value Pp_utils.pretty_printer;;
val show_value : value -> string;;
