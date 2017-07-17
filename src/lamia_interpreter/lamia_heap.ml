open Batteries;;
open Jhupllib;;

open Lamia_evaluation_ast;;
open Lamia_evaluation_grammar;;

module Value_variable_map =
struct
  module Impl = Map.Make(Value_variable);;
  include Impl;;
  include Pp_utils.Map_pp(Impl)(Value_variable);;
end;;

module Memory_variable_map =
struct
  module Impl = Map.Make(Memory_variable);;
  include Impl;;
  include Pp_utils.Map_pp(Impl)(Memory_variable);;
end;;

module Memory_address_map =
struct
  module Impl = Map.Make(Memory_address);;
  include Impl;;
  include Pp_utils.Map_pp(Impl)(Memory_address);;
end;;

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
  val to_string : t -> string;;
end =
struct
  type t =
    { x_map : value Value_variable_map.t;
      y_map : memory_address Memory_variable_map.t;
      m_map : value Memory_address_map.t;
    }
  [@@deriving show]
  ;;

  let empty =
    { x_map = Value_variable_map.empty;
      y_map = Memory_variable_map.empty;
      m_map = Memory_address_map.empty;
    };;

  let get_value x h = Value_variable_map.find x h.x_map;;
  let get_memory_address y h = Memory_variable_map.find y h.y_map;;
  let get_heap_value m h = Memory_address_map.find m h.m_map;;
  let set_value x v h =
    { h with
      x_map = Value_variable_map.add x v h.x_map
    };;
  let set_memory_address y m h =
    { h with
      y_map = Memory_variable_map.add y m h.y_map
    };;
  let set_heap_value m v h =
    { h with
      m_map = Memory_address_map.add m v h.m_map
    };;

  let to_string h =
    show h
  ;;
end;;
