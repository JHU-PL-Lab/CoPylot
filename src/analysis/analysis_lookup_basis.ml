open Batteries;;
open Analysis_grammar;;

module State =
struct
  type t =
    | Program_state of Program_state.t
    (* | Answer_value of  *)
  (* | Answer_memory of  *)
  [@@deriving eq, ord, show, to_yojson]
  ;;
end;;

module Stack_element =
struct
  type t =
    | Bottom
    | Lookup_value_variable
    | Lookup_memory_variable
    | Lookup_value
    | Lookup_memory
    | Lookup_capture of int
    | Lookup_jump of Program_state.t
    | Lookup_bind
    | Lookup_project
    | Lookup_index
    | Lookup_slice
    | Lookup_isalias
    | Lookup_dereference
    | Lookup_unop
    | Lookup_binop
    | Lookup_answer
    | Lookup_is
  [@@deriving eq, ord, show, to_yojson]
  ;;
end;;

module Basis =
struct
  module State = State
  module Stack_element = Stack_element
end;;
