open Batteries;;
open Jhupllib;;
open Analysis_lookup_basis;;
open Analysis_types;;
open Analysis_grammar;;

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
      | Tdp_store of memory_variable * Program_state.t
      | Tdp_unop of unary_operator * value_variable
      (* | Tdp_conditional_value of value_variable *)
    [@@deriving eq, ord, show, to_yojson]
    ;;
  end;;
  module Untargeted_dynamic_pop_action =
  struct
    type t =
      | Udp_jump
      | Udp_ifresult_x of value_variable * State.t * Program_state.t
      | Udp_ifresult_y of memory_variable * State.t * Program_state.t
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
      begin
        let%orzero Tdp_store (y,o0) = action in
        let%orzero Lookup_memory _ = element in
        return [Pop(Lookup_dereference); Push(Lookup_dereference); Push (element); Push (Lookup_isalias); Push (Lookup_jump o0); Push (Lookup_capture 2); Push(Lookup_memory_variable y)]
      end;
      (* TODO: unop function? *)
      (* begin
        let%orzero Tdp_conditional_value x = action in
        let%orzero Lookup_value_variable x' = element in
        if x = x' then
          return [Push (Lookup_answer)]
        else
          return [Push (Lookup_value_variable x')]
      end; *)
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
    | Udp_ifresult_x (x,body,skip) ->
      begin
        match element with
        | Lookup_value_variable x' ->
          if x = x' then
            Enum.singleton ([Push (Lookup_answer)], Static_terminus(body))
          else
            Enum.singleton ([Push (element)], Static_terminus(Program_state skip))
        | _ -> Enum.empty ()
      end
    | Udp_ifresult_y (y,body,skip) ->
      begin
        match element with
        | Lookup_memory_variable y' ->
          if y = y' then
            Enum.singleton ([Push (Lookup_answer)], Static_terminus(body))
          else
            Enum.singleton ([Push (element)], Static_terminus(Program_state skip))
        | _ -> Enum.empty ()
      end
  ;;
end;;
