open Analysis_types;;

module Program_state =
struct
  type t =
    | Start
    | End
    | Stmt of statement
    | Advance of statement
    | Ifresult of statement
    | Return of statement
    | Raise of statement
  [@@deriving eq, ord, show]
  ;;
end;;

module Cfg = Graph.Make(Program_state)
