open Batteries;;
open Analysis_grammar;;
open Analysis_types;;
open Analysis_lexical_relations;;

type pds;;
val empty : relation_map_record -> pds;;
val add_cfg_edge : Cfg.edge -> pds -> pds;;
val lookup_value : Program_state.t -> value_variable -> pds -> value Enum.t * pds;;
val lookup_memory : Program_state.t -> memory_variable -> pds -> value Enum.t * pds;;
val lookup_memory_location : Program_state.t -> memory_location -> pds -> value Enum.t * pds;;
