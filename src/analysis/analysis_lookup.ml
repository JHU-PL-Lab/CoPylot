open Batteries;;
(* open Jhupllib;; *)
open Analysis_grammar;;
open Analysis_lookup_basis;;
open Analysis_lookup_dph;;
open Analysis_lookup_edge_functions;;
open Analysis_lexical_relations;;

open State;;
(* open Program_state;; *)
open Stack_element;;
(* open Dph.Terminus.T;; *)
open Dph.Stack_action.T;;
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

let get_value state =
  match state with
  | Program_state _ | Answer_memory _ -> None
  | Answer_value v -> Some v
;;

let perform_lookup start_state start_actions pds =
  let analysis' = Reachability.add_start_state start_state start_actions pds.lookup_analysis in
  let closed = Reachability.fully_close analysis' in
  let reachables = Reachability.get_reachable_states start_state start_actions closed in
  let values = Enum.filter_map get_value reachables in
  (values, {pds with lookup_analysis = closed})
;;

let lookup_value ps x pds =
  let start_state = Program_state ps in
  let start_actions = [Push Bottom; Push (Lookup_value_variable x)] in
  perform_lookup start_state start_actions pds
;;

let lookup_memory ps y pds =
  let start_state = Program_state ps in
  let start_actions = [Push Bottom; Push Lookup_dereference; Push (Lookup_jump ps); Push (Lookup_capture 1); Push (Lookup_memory_variable y)] in
  perform_lookup start_state start_actions pds
;;

let lookup_memory_location ps m pds =
  let start_state = Program_state ps in
  let start_actions = [Push Bottom; Push Lookup_dereference; Push (Lookup_memory m)] in
  perform_lookup start_state start_actions pds
;;

let lookup_in_scope ps target pds =
  let open Analysis_types in
  let start_state = Program_state ps in
  let start_actions =
    [
      Push Bottom;

      Push (Lookup_dereference);
      Push (Lookup_jump ps);
      Push (Lookup_capture 1);

      Push (Lookup_value (String_value (String_exact target)));
      Push (Lookup_project);
      Push (Lookup_capture 1);

      Push (Lookup_dereference);
      Push (Lookup_jump ps);
      Push (Lookup_capture 1);

      Push (Lookup_memory_variable (Memory_variable "&scope"));
    ]
  in
  perform_lookup start_state start_actions pds
;;

let lookup_to_starvalue ps target pds =
  let open Analysis_types in
  let start_state = Program_state ps in
  let start_actions =
    [
      Push Bottom;

      Push (Lookup_dereference);
      Push (Lookup_jump ps);
      Push (Lookup_capture 1);

      Push (Lookup_value (String_value (String_exact "*value")));
      Push (Lookup_project);
      Push (Lookup_capture 1);

      Push (Lookup_dereference);
      Push (Lookup_jump ps);
      Push (Lookup_capture 1);

      Push (Lookup_value (String_value (String_exact target)));
      Push (Lookup_project);
      Push (Lookup_capture 1);

      Push (Lookup_dereference);
      Push (Lookup_jump ps);
      Push (Lookup_capture 1);

      Push (Lookup_memory_variable (Memory_variable "&scope"));
    ]
  in
  perform_lookup start_state start_actions pds
;;
