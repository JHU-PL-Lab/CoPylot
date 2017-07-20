open Batteries;;
open Jhupllib;;
open Analysis_lookup_basis;;
open Analysis_types;;
(* open Analysis_grammar;; *)

open Nondeterminism;;
open Pds_reachability_types_stack;;

module Dph =
struct
  module State = State
  module Stack_element = Stack_element
  module Targeted_dynamic_pop_action =
  struct
    type t =
      | Tdp_capture_1
      | Tdp_capture_2 of value
      | Tdp_capture_3 of value * int * Stack_element.t list
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
  open Targeted_dynamic_pop_action;;
  open Untargeted_dynamic_pop_action;;
  open Stack_action.T;;
  open Terminus.T;;

  let perform_targeted_dynamic_pop element action =
    let open Nondeterminism_monad in
    [
      begin
        let%orzero Tdp_capture_1 = action in
        let%orzero Lookup_value v = element in
        return [Pop_dynamic_targeted (Tdp_capture_2 v)]
      end;
      begin
        let%orzero Tdp_capture_2 v = action in
        let%orzero Lookup_capture n = element in
        return [Pop_dynamic_targeted (Tdp_capture_3 (v,n,[]))]
      end;
      begin
        let%orzero Tdp_capture_3 (v,n,lst) = action in
        if n > 1 then
          return [Pop_dynamic_targeted (Tdp_capture_3 (v,n-1,element::lst))]
        else
          return @@ [Push (Lookup_value v); Push element] @ List.map (fun x -> Push x) (lst)
      end;
    ]
    |> List.enum
    |> Enum.map Nondeterminism_monad.enum
    |> Enum.concat
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
