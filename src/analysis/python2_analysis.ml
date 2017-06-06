open Batteries;;
open Python2_cfg;;
open Python2_pds;;
open Python2_normalized_ast;;
open Python2_control_rules;;
open Python2_pds_edge_functions;;
open Python2_pds.Reachability.Stack_action.T;;
open Python2_uid_stmt_map;;

module Analysis_result =
struct
  type t =
    {
      analysis_cfg: Cfg.t;
      analysis_pds: pds;
    }

  let add_edge (curr : t) (e : Control_cfg.edge) : t =
    let new_cfg = Cfg.add_control_edge e curr.analysis_cfg in
    let new_pds = Python2_pds.Reachability.add_edge_function
        (create_edge_function e) curr.analysis_pds in
    { analysis_cfg = new_cfg; analysis_pds = new_pds }

  let query
      (analysis : t) (prog_point : vertex) (var : identifier) : Answer_set.t =
    let start_state = Cfg_node(prog_point) in
    let start_actions = [Push Bottom; Push (Var(var))] in
    let final_pds =
      analysis.analysis_pds
      |> Reachability.add_start_state start_state start_actions
      |> Reachability.fully_close
    in
    let values =
      final_pds
      |> Reachability.get_reachable_states start_state start_actions
      |> Enum.filter_map
        (function
          | Value_node v -> Some v
          | _ -> None)
    in
    Answer_set.of_enum values
  ;;

  let rec build_cfg_and_pds (curr : t) : t =
    let edges_to_add = get_edges_to_add curr.analysis_cfg in
    if Enum.is_empty edges_to_add then
      curr
    else
    let result = Enum.fold add_edge curr edges_to_add in
    build_cfg_and_pds result
;;

  let create (prog : modl) : t =
    let cfg = Cfg.create prog in
    let pds =
      Python2_pds.Reachability.empty ()
      |> Python2_pds.Reachability.add_edge_function (value_loop_edge_function)
    in
    build_cfg_and_pds ({ analysis_cfg = cfg; analysis_pds = pds })

end;;

(* TODO: Take an option between uid and End *)
let analyze_uid (prog : modl) (prog_point : uid) (var : identifier) : Answer_set.t =
  let analysis = Analysis_result.create prog in
  let uid_to_stmt_map = get_uid_hashtbl prog in
  let prog_point_stmt = Uid_generation.Uid_hashtbl.find uid_to_stmt_map prog_point in
  Analysis_result.query analysis (Program_point(prog_point_stmt)) var
;;

let analyze_end (prog : modl) (var : identifier) : Answer_set.t =
  let analysis = Analysis_result.create prog in
  Analysis_result.query analysis End var
;;
