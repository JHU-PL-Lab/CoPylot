open Batteries;;
open Analysis_grammar;;
open Analysis_types;;

type pds;;
val empty : unit -> pds;;
val add_cfg_edge : Cfg.edge -> pds -> pds;;
val lookup_value : Program_state.t -> value_variable -> pds -> value Enum.t * pds;;
val lookup_memory : Program_state.t -> memory_variable -> pds -> value Enum.t * pds;;
val lookup_memory_location : Program_state.t -> memory_location -> pds -> value Enum.t * pds;;
