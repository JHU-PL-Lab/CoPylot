open Batteries;;
open Python2_normalized_ast;;
open Pds_reachability_types_stack;;

type answer =
  | Num of number
  | Str of str
  | Bool of bool
[@@deriving eq, ord, show, to_yojson]
;;

module Answer =
struct
  type t = answer
  let compare = compare_answer;;
end;;

module Answer_set = Set.Make(Answer);;

type state =
  | Program_point of stmt
[@@deriving eq, ord, show, to_yojson]
;;

module State =
struct
  type t = state
  let equal = equal_state
  let compare = compare_state
  let pp = pp_state
  let show = show_state
  let to_yojson = state_to_yojson
end;;

type stack_elt =
  | Bottom
  | Ans of answer
  | Var of identifier
  (* TODO: Binops *)
[@@deriving eq, ord, show, to_yojson]
;;

module Stack_element =
struct
  type t = stack_elt
  let equal = equal_stack_elt
  let compare = compare_stack_elt
  let pp = pp_stack_elt
  let show = show_stack_elt
  let to_yojson = stack_elt_to_yojson
end;;

module Spec =
struct
  module State = State
  module Stack_element = Stack_element
end;;

module Dph =
struct
  module State = State
  module Stack_element = Stack_element
  type targeted_dynamic_pop_action =
    | Pop_anything_but of stack_elt
    [@@deriving eq, ord, show, to_yojson]
  ;;
  module Targeted_dynamic_pop_action =
  struct
    type t = targeted_dynamic_pop_action;;
    let equal = equal_targeted_dynamic_pop_action;;
    let compare = compare_targeted_dynamic_pop_action;;
    let pp = pp_targeted_dynamic_pop_action;;
    let show = show_targeted_dynamic_pop_action;;
    let to_yojson = targeted_dynamic_pop_action_to_yojson;;
  end;;
  module Untargeted_dynamic_pop_action =
  struct
    type t = Foo of t
      [@@deriving eq, ord, show, to_yojson]
  end;;
  module Stack_action =
    Stack_action_constructor(Stack_element)(Targeted_dynamic_pop_action)
  ;;
  module Terminus =
    Terminus_constructor(State)(Untargeted_dynamic_pop_action)
  ;;
  let perform_targeted_dynamic_pop element action =
    match action with
    | Pop_anything_but se ->
      if equal_stack_elt element se then
        Enum.empty ()
      else
        Enum.singleton []
  ;;
  let perform_untargeted_dynamic_pop _ (* element *) _ (* action *) =
    Enum.empty ()
  ;;
end;;

module Reachability =
  (*TODO: there appears to be a problem here with not having untargeted dynamic pops, and also with pretty printing*)
  Pds_reachability.Make
    (Spec)
    (Dph)
    (Pds_reachability_work_collection_templates.Work_stack)
;;

type pds = Reachability.analysis;;
