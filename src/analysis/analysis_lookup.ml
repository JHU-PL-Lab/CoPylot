open Batteries;;
open Jhupllib;;
open Analysis_grammar;;
open Analysis_lookup_basis;;
open Analysis_lookup_dph;;
open Analysis_lookup_edge_functions;;
open Analysis_lexical_relations;;

(* open Pds_reachability_types_stack;; *)

module Reachability =
  Pds_reachability.Make
    (Basis)
    (Dph)
    (Pds_reachability_work_collection_templates.Work_stack)
;;

type pds =
  {
    lookup_analysis: Reachability.analysis;
    relations: relation_map_record
  };;

let empty rmr =
  let analysis =
  Reachability.empty ()
  |> Reachability.add_edge_function global_edge_function
  in
  {lookup_analysis = analysis; relations = rmr}
;;

let add_cfg_edge edge pds =
  let Cfg.Edge (src, dst) = edge in
  let rmr = pds.relations in
  let edge_function = per_cfg_edge_function rmr src dst in
  let analysis' = Reachability.add_edge_function edge_function pds.lookup_analysis in
  {pds with lookup_analysis = analysis'}
;;

let lookup_value ps x pds =
  ignore ps; ignore x; ignore pds;
  raise @@ Utils.Not_yet_implemented "lookup_value"
;;

let lookup_memory ps y pds =
  ignore ps; ignore y; ignore pds;
  raise @@ Utils.Not_yet_implemented "lookup_memory"
;;

let lookup_memory_location ps m pds =
  ignore ps; ignore m; ignore pds;
  raise @@ Utils.Not_yet_implemented "lookup_memory_location"
;;
