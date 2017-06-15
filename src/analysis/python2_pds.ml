open Batteries;;
open Jhupllib;;
open Nondeterminism;;
open Python2_ast_types;;
open Python2_abstract_ast;;
open Pds_reachability_types_stack;;
open Python2_cfg;;

type answer =
  | Undefined
  | Num of number
  | Str of str
  | Bool of bool
  | Builtin of builtin
  | NoneVal 
[@@deriving eq, ord, show, to_yojson]
;;

module Answer =
struct
  type t = answer
  let compare = compare_answer;;
end;;

module Answer_set = Set.Make(Answer);;

type state =
  | Cfg_node of vertex
  | Value_node of answer
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
    | Pop_then_push_any_variable_but of stack_elt
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
    type t =
      | Goto_value_state
      | Pop_undefined_variable
    [@@deriving eq, ord, show, to_yojson]
  end;;
  module Stack_action =
    Stack_action_constructor(Stack_element)(Targeted_dynamic_pop_action)
  ;;
  module Terminus =
    Terminus_constructor(State)(Untargeted_dynamic_pop_action)
  ;;

  open Stack_action.T;;
  open Terminus.T;;
  open Untargeted_dynamic_pop_action;;

  let perform_targeted_dynamic_pop element action =
    match action with
    | Pop_anything_but se ->
      if equal_stack_elt element se then
        Enum.empty ()
      else
        Enum.singleton []

    | Pop_then_push_any_variable_but se ->
      begin
        match element with
        | Var _ ->
          if equal_stack_elt element se
          then Enum.empty ()
          else Enum.singleton [Push (element)]
        | _ -> Enum.empty ()
      end
  ;;
  let perform_untargeted_dynamic_pop element action =
    let open Nondeterminism_monad in
    Nondeterminism_monad.enum @@
    match action with
    | Goto_value_state ->
      let%orzero
        Ans(v) = element
      in
      return ([], Static_terminus(Value_node(v)))
    | Pop_undefined_variable ->
      let%orzero
        Var(_) = element
      in
      return ([], Static_terminus(Value_node(Undefined)))
  ;;
end;;

module Reachability =
  Pds_reachability.Make
    (Spec)
    (Dph)
    (Pds_reachability_work_collection_templates.Work_stack)
;;

type pds = Reachability.analysis;;

let query_pds
    (p : pds) (prog_point : vertex) (var : identifier) : Answer_set.t =
  let start_state = Cfg_node(prog_point) in
  let open Reachability.Stack_action.T in
  let start_actions = [Push Bottom; Push (Var(var))] in
  let final_pds =
    p
    |> Reachability.add_start_state start_state start_actions
    |> Reachability.fully_close
    (* TODO: This is the only place we close, so we're doing a lot of
       duplicate work. Would be better to close before calling query *)
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
