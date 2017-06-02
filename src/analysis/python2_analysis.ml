open Batteries;;
open Python2_cfg;;
open Python2_pds;;
open Python2_normalized_ast;;

module Analysis_result =
struct
  type t =
    {
      analysis_cfg: cfg;
      analysis_pds: pds;
    }

  let create (prog : modl) : t = ignore prog; raise Utils.Not_yet_implemented;;

  let query
      (analysis : t) (prog_point : uid) (var : identifier) : Answer_set.t =
    ignore analysis; ignore prog_point; ignore var;
    raise Utils.Not_yet_implemented;;
end;;

let analyze (prog : modl) (prog_point : uid) (var : identifier) : Answer_set.t =
  let analysis = Analysis_result.create prog in
  Analysis_result.query analysis prog_point var
;;
