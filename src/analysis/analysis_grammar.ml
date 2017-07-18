open Lamia_ast

module Program_state =
struct
  type t =
    | Start
    | End
    | Stmt of uid statement
    | Advance of uid statement
    | Ifresult of uid statement
    | Return of uid statement
    | Raise of uid statement
  [@@deriving eq, ord, show]
  ;;
end;;

module Cfg = Graph.Make(Program_state)
