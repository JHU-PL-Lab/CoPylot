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
      | Tdp_peek_x
      | Tdp_peek_y
      | Tdp_capture_1
      | Tdp_capture_2 of value
      | Tdp_capture_3 of value * int * Stack_element.t list
      | Tdp_bind_1
      | Tdp_bind_2 of memory_location AbstractStringMap.t
      | Tdp_bind_3 of memory_location AbstractStringMap.t * abstract_str
      | Tdp_project_1
      | Tdp_project_2 of memory_location AbstractStringMap.t
      | Tdp_index_1
      | Tdp_index_2 of abstract_memloc_list
      | Tdp_store of memory_variable * Program_state.t
      | Tdp_unop of unary_operator * value_variable
      (* | Tdp_conditional_value of value_variable *)
    [@@deriving eq, ord, show, to_yojson]
    ;;
  end;;
  module Untargeted_dynamic_pop_action =
  struct
    type t =
      | Udp_result
      | Udp_jump
      | Udp_ifresult_x of value_variable * State.t * Program_state.t
      | Udp_ifresult_y of memory_variable * State.t * Program_state.t
      | Udp_return of memory_variable * State.t * Program_state.t
      | Udp_raise of memory_variable * State.t * Program_state.t
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
      (* Capture steps *)
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

      (* Bind steps *)
      begin
        let%orzero Tdp_bind_1 = action in
        let%orzero Lookup_value(Object_value v) = element in
        return [Pop_dynamic_targeted (Tdp_bind_2 v)]
      end;
      begin
        let%orzero Tdp_bind_2 v = action in
        let%orzero Lookup_value(String_value str) = element in
        return [Pop_dynamic_targeted (Tdp_bind_3 (v,str))]
      end;
      begin
        let%orzero Tdp_bind_3 (v,str) = action in
        let%orzero Lookup_memory m = element in
        let new_binding = AbstractStringMap.add str m v in
        return [Push(Lookup_value (Object_value new_binding))]
      end;

      (* Project steps *)
      begin
        let%orzero Tdp_project_1 = action in
        let%orzero Lookup_value(Object_value v) = element in
        return [Pop_dynamic_targeted (Tdp_project_2 v)]
      end;

      begin
        let%orzero Tdp_project_2 v = action in
        let%orzero Lookup_value(String_value str) = element in
        let%orzero Some(m) = AbstractStringMap.Exceptionless.find str v in
        return [Push(Lookup_memory m)]
      end;

      (* Index steps *)
      begin
        let%orzero Tdp_index_1 = action in
        let%orzero Lookup_value(List_value lst) = element in
        return [Pop_dynamic_targeted (Tdp_index_2 lst)]
      end;

      (* begin
        let%orzero Tdp_index_2 lst = action in
        let%orzero Lookup_value(Integer_value i) = element in
        match i with
        | Neg -> return []
        (* match lst with
        | List_exact le ->
          let m = List.nth le i in
          return [Push(Lookup_memory m)]
        | List_lossy ll ->
          let m = List.nth ll i in
          return [Push(Lookup_memory m)] *)
      end; *)

      (* Store *)
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
      begin
        let%orzero Tdp_peek_x = action in
        let%orzero Lookup_value_variable _ = element in
        return [Pop(element); Push (element)]
      end;
      begin
        let%orzero Tdp_peek_y = action in
        let%orzero Lookup_memory_variable _ = element in
        return [Pop(element); Push (element)]
      end;
    ]
    |> List.enum
    |> Enum.map Nondeterminism_monad.enum
    |> Enum.concat
  ;;

  let perform_untargeted_dynamic_pop element action =
    match action with
    | Udp_result ->
      begin
        match element with
        | Lookup_value v -> Enum.singleton ([Pop(Bottom)], Static_terminus(Answer_value v))
        | _ -> Enum.empty ()
      end
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
    (* TODO: combine the following? *)
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
    | Udp_return (y,body,skip) ->
      begin
        match element with
        | Lookup_memory_variable y' ->
          if y = y' then
            Enum.singleton ([Push (Lookup_answer)], Static_terminus(body))
          else
            Enum.singleton ([Push (element)], Static_terminus(Program_state skip))
        | _ -> Enum.empty ()
      end
    | Udp_raise (y,body,skip) ->
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
