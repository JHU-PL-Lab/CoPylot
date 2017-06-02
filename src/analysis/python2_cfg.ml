open Batteries;;
open Python2_normalized_ast;;

type cfg_lexical_edge =
  | Cfg_lexical_edge of stmt * stmt
[@@deriving eq, ord, show]
;;

type cfg_control_edge =
  | Cfg_control_edge of stmt * stmt
[@@deriving eq, ord, show]
;;

module Cfg_lexical_edge =
struct
  type t = cfg_lexical_edge
  let compare = compare_cfg_lexical_edge
  let pp = pp_cfg_lexical_edge
  let equal = equal_cfg_lexical_edge
end;;

module Cfg_control_edge =
struct
  type t = cfg_control_edge
  let compare = compare_cfg_control_edge
  let pp = pp_cfg_control_edge
  let equal = equal_cfg_control_edge
end;;

module Lexical_cfg = Graph.Make(Cfg_lexical_edge);;

module Control_cfg = Graph.Make(Cfg_control_edge);;

type cfg = Cfg of Lexical_cfg.t * Control_cfg.t;;
