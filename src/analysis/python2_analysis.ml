open Batteries;;
open Python2_cfg;;
open Python2_pds;;
open Python2_normalized_ast;;

module Analysis_result =
struct
  type t =
    {
      analysis_cfg: Cfg.t;
      analysis_pds: pds;
    }

  let add_edge (curr : t) (e : Control_cfg.edge) : t =
    let new_cfg = Cfg.add_control_edge e curr.analysis_cfg in
    let new_pds = curr.analysis_pds in (* TODO: Edge functions *)
    { analysis_cfg = new_cfg; analysis_pds = new_pds }

  let query
      (analysis : t) (prog_point : uid) (var : identifier) : Answer_set.t =
    (* Call closure *) ignore analysis; ignore prog_point; ignore var;
    raise Utils.Not_yet_implemented;;

  let rec build_cfg_and_pds (curr : t) : t =
    let edges_to_add = Cfg.get_edges_to_add curr.analysis_cfg in
    match edges_to_add with
    | [] -> curr
    | _ ->
      let result = List.fold_left add_edge curr edges_to_add in
      build_cfg_and_pds result

  let create (prog : modl) : t =
    let cfg = Cfg.create prog in
    let pds = Python2_pds.Reachability.empty () in
    build_cfg_and_pds ({ analysis_cfg = cfg; analysis_pds = pds })

end;;

let analyze (prog : modl) (prog_point : uid) (var : identifier) : Answer_set.t =
  let analysis = Analysis_result.create prog in
  Analysis_result.query analysis prog_point var
;;
