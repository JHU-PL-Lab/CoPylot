open Batteries;;
open Jhupllib;;
open Analysis_lookup_basis;;
(* open Analysis_grammar;; *)

open Pds_reachability_types_stack;;

module Dph =
struct
  module State = State
  module Stack_element = Stack_element
  module Targeted_dynamic_pop_action =
  struct
    type t =
      | Todo of t
    [@@deriving eq, ord, show, to_yojson]
    ;;
  end;;
  module Untargeted_dynamic_pop_action =
  struct
    type t =
      | Udp_jump
    [@@deriving eq, ord, show, to_yojson]
  end;;
  module Stack_action =
    Stack_action_constructor(Stack_element)(Targeted_dynamic_pop_action)
  ;;
  module Terminus =
    Terminus_constructor(State)(Untargeted_dynamic_pop_action)
  ;;

  open Stack_element;;
  open State;;
  open Untargeted_dynamic_pop_action;;
  (* open Stack_action.T;; *)
  open Terminus.T;;

  let perform_targeted_dynamic_pop element action =
    ignore element; ignore action;
    raise @@ Utils.Not_yet_implemented "perform_targeted_dynamic_pop"
  ;;
  let perform_untargeted_dynamic_pop element action =
    match action with
    | Udp_jump ->
      begin
        match element with
        | Lookup_jump state -> Enum.singleton ([], Static_terminus(Program_state state))
        | _ -> Enum.empty ()
      end

  ;;
end;;
