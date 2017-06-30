open Python2_cfg;;
open Python2_pds;;
open Python2_normalization_ctx;;
open Python2_abstract_ast;;

type analysis_result =
{
  analysis_cfg: Cfg.t;
  analysis_pds: pds;
  analysis_uid_map : annotated_stmt Counter_hashtbl.t;
}
