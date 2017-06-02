open Batteries;;
open Python2_normalized_ast;;

type answer =
  | Num of number
  | Str of str
  | Bool of bool
[@@deriving ord]
;;

module Answer =
struct
  type t = answer
  let compare = compare_answer;;
end;;

module Answer_set = Set.Make(Answer);;

type pds = int;; (* FIXME: Obvious *)
