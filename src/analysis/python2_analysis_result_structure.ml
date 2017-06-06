open Python2_cfg;;
open Python2_pds;;
open Uid_generation;;
open Python2_normalized_ast;;

type analysis_result =
{
  analysis_cfg: Cfg.t;
  analysis_pds: pds;
  analysis_uid_map : stmt Uid_hashtbl.t;
}
