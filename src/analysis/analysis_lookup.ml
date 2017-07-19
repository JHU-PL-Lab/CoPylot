open Batteries;;
open Jhupllib;;
(* open Analysis_grammar;; *)
open Analysis_lookup_basis;;
open Analysis_lookup_dph;;

(* open Pds_reachability_types_stack;; *)

module Reachability =
  Pds_reachability.Make
    (Basis)
    (Dph)
    (Pds_reachability_work_collection_templates.Work_stack)
;;

type pds = Reachability.analysis;;

let empty () =
  raise @@ Utils.Not_yet_implemented "empty"
;;

let add_cfg_edge edge pds =
  ignore edge; ignore pds;
  raise @@ Utils.Not_yet_implemented "add_cfg_edge"
;;

let lookup_value ps x pds =
  ignore ps; ignore x; ignore pds;
  raise @@ Utils.Not_yet_implemented "lookup_value"
;;

let lookup_memory ps y pds =
  ignore ps; ignore y; ignore pds;
  raise @@ Utils.Not_yet_implemented "lookup_memory"
;;
