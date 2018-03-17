open Batteries;;
open Jhupllib;;

open Lamia_evaluation_ast;;

module PpString =
struct
  type t = string
  let compare = String.compare
  let pp = Format.pp_print_string;;
end;;
module RawStringMap = Map.Make(PpString);;

module StringMap :
sig
  include module type of RawStringMap;;
  val pp : 'a Pp_utils.pretty_printer ->
    'a RawStringMap.t Pp_utils.pretty_printer;;
end =
struct
  include RawStringMap;;
  include Pp_utils.Map_pp(RawStringMap)(PpString);;
end;;

type memory_address =
  Memory_address of int
[@@deriving eq, ord, show]
;;

module Memory_address =
struct
  type t =
    memory_address
  [@@deriving eq, ord, show]
  ;;
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
[@@deriving eq, ord, show]
;;
