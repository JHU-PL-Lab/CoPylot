open Batteries;;
open Jhupllib;;
open Analysis_lookup_basis;;
open Analysis_types;;
open Analysis_grammar;;
open Analysis_lookup_utils;;

open Logger_utils;;
open Program_state;;
open Nondeterminism;;
open Pds_reachability_types_stack;;

set_default_logging_level `debug;;
let logger = make_logger "Analysis_lookup_dph";;

module Dph =
struct
  module State = State
  module Stack_element = Stack_element
  module Targeted_dynamic_pop_action =
  struct
    type t =
      | Tdp_peek_x of value_variable option
      | Tdp_peek_y of memory_variable option
      | Tdp_peek_m_1
      | Tdp_peek_m_2 of Stack_element.t
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
      | Tdp_isalias_1 of value_variable
      | Tdp_isalias_2 of value_variable * memory_location
      | Tdp_is_1
      | Tdp_is_2 of memory_location
      | Tdp_func_search of value_variable * value_variable list
      | Tdp_unop_2 of unary_operator
      | Tdp_binop_2 of binary_operator
      | Tdp_binop_3 of binary_operator * value
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
      | Udp_advance of statement * State.t
      | Udp_unop_1 of unary_operator * value_variable * Program_state.t * State.t * State.t
      | Udp_binop_1 of binary_operator * value_variable * value_variable * Program_state.t * State.t * State.t
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
      begin
        let%orzero Tdp_index_2 lst = action in
        let%orzero Lookup_value(Integer_value i) = element in
        match i with
        | Neg -> return []
        | _ ->
          begin
            match lst with
            | List_exact le ->
              let m = List.hd le in
              return [Push(Lookup_memory m)]
            | List_lossy ll ->
              let m = List.hd ll in
              return [Push(Lookup_memory m)]
          end
      end;

      (* Store *)
      begin
        let%orzero Tdp_store (y,o0) = action in
        let%orzero Lookup_memory _ = element in
        return [Pop(Lookup_dereference); Push(Lookup_dereference); Push (element); Push (Lookup_isalias); Push (Lookup_jump o0); Push (Lookup_capture 2); Push(Lookup_memory_variable y)]
      end;

      (* begin
         let%orzero Tdp_conditional_value x = action in
         let%orzero Lookup_value_variable x' = element in
         if x = x' then
          return [Push (Lookup_answer)]
         else
          return [Push (Lookup_value_variable x')]
         end; *)

      begin
        let%orzero Tdp_peek_x x = action in
        let%orzero Lookup_value_variable (Value_variable id) = element in
        match x with
        | None ->
          return [Pop(element); Push (element)]
        | Some Value_variable id' ->
          if id != id' then
            return [Pop(element); Push (element)]
          else
            return [Nop]
      end;
      begin
        let%orzero Tdp_peek_y y = action in
        let%orzero Lookup_memory_variable (Memory_variable id) = element in
        match y with
        | None ->
          return [Pop(element); Push (element)]
        | Some Memory_variable id' ->
          if id != id' then
            return [Pop(element); Push (element)]
          else
            return [Nop]
      end;

      (* Peek dereference steps *)
      begin
        let%orzero Tdp_peek_m_1 = action in
        let%orzero Lookup_memory _ = element in
        return [Pop_dynamic_targeted(Tdp_peek_m_2 element)]
      end;
      begin
        let%orzero Tdp_peek_m_2 element' = action in
        let%orzero Lookup_dereference = element in
        return [Push(element); Push(element')]
      end;

      (* Isalias steps *)
      begin
        let%orzero Tdp_isalias_1 x = action in
        let%orzero Lookup_memory m = element in
        return [Pop_dynamic_targeted(Tdp_isalias_2 (x,m))]
      end;
      begin
        let%orzero Tdp_isalias_2 (x,m) = action in
        let%orzero Lookup_memory m' = element in
        if m = m' then
          return [Pop(Lookup_dereference); Push(Lookup_dereference); Push(Lookup_value_variable x)]
        else
          return [Pop(Lookup_dereference); Push(Lookup_dereference); Push(element)]
      end;

      (* Is steps *)
      begin
        let%orzero Tdp_is_1 = action in
        let%orzero Lookup_memory m = element in
        return [Pop_dynamic_targeted(Tdp_is_2 m)]
      end;
      begin
        let%orzero Tdp_is_2 m = action in
        let%orzero Lookup_memory m' = element in
        if m = m' then
          return [Push(Lookup_value (Boolean_value true))]
        else
          return [Push(Lookup_value (Boolean_value false))]
      end;

      (* Function search *)
      begin
        let%orzero Tdp_func_search (x0,lst') = action in
        let%orzero Lookup_value_variable xi = element in
        if List.mem xi lst' then
          return [Push(Lookup_value_variable xi)]
        else
          return [Push (Lookup_value_variable xi); Push (Lookup_drop); Push (Lookup_value_variable x0)]
      end;

      (* Unop *)
      begin
        let%orzero Tdp_unop_2 op = action in
        let%orzero Lookup_value v = element in
        let%bind v' = pick_enum(unary_operation op v) in
        return [Push(Lookup_value v')]
      end;

      (* Binop steps *)
      begin
        let%orzero Tdp_binop_2 op = action in
        let%orzero Lookup_value v1 = element in
        return [Pop_dynamic_targeted(Tdp_binop_3 (op,v1))]
      end;
      begin
        let%orzero Tdp_binop_3 (op,v1) = action in
        let%orzero Lookup_value v2 = element in
        let%bind v = pick_enum (binary_operation op v1 v2) in
        return [Push(Lookup_value v)]
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
    | Udp_ifresult_x (x,prev,skip) ->
      begin
        match element with
        | Lookup_value_variable x' ->
          if x = x' then
            Enum.singleton ([Push (Lookup_answer)], Static_terminus(prev))
          else
            Enum.singleton ([Push (element)], Static_terminus(Program_state skip))
        | Lookup_answer ->
          Enum.singleton ([Push (element)], Static_terminus(prev))
        | _ -> Enum.empty ()
      end
    (* TODO: combine the following? *)
    | Udp_ifresult_y (y,prev,skip) ->
      begin
        match element with
        | Lookup_memory_variable y' ->
          if y = y' then
            Enum.singleton ([Push (Lookup_answer)], Static_terminus(prev))
          else
            Enum.singleton ([Push (element)], Static_terminus(Program_state skip))
        | Lookup_answer ->
          Enum.singleton ([Push (element)], Static_terminus(prev))
        | _ -> Enum.empty ()
      end
    | Udp_return (y,prev,skip) ->
      begin
        match element with
        | Lookup_memory_variable y' ->
          if y = y' then
            Enum.singleton ([Push (Lookup_answer)], Static_terminus(prev))
          else
            Enum.singleton ([Push (element)], Static_terminus(Program_state skip))
        | Lookup_answer ->
          Enum.singleton ([Push (element)], Static_terminus(prev))
        | _ -> Enum.empty ()
      end
    | Udp_raise (y,prev,skip) ->
      begin
        match element with
        | Lookup_memory_variable y' ->
          if y = y' then
            Enum.singleton ([Push (Lookup_answer)], Static_terminus(prev))
          else
            Enum.singleton ([Push (element)], Static_terminus(Program_state skip))
        | Lookup_answer ->
          Enum.singleton ([Push (element)], Static_terminus(prev))
        | _ -> Enum.empty ()
      end
    | Udp_advance (target, prev) ->
      begin
        match target with
        | Statement(_, Try_except _) ->
          let () = logger `debug "skip try/except" in
          Enum.singleton ([Push (element)], Static_terminus(Program_state (Stmt target)))
        | _ ->
          let () = logger `debug "normal advance" in
          Enum.singleton ([Push (element)], Static_terminus(prev))
      end
    | Udp_unop_1 (op,x',dst,o0,o1) ->
      begin
        match element with
        | Lookup_value_variable _ ->
          Enum.singleton ([Push (Lookup_unop); Push (Lookup_jump dst); Push (Lookup_capture 3); Push(Lookup_value_variable x')], Static_terminus(o0))
        | Lookup_unop ->
          Enum.singleton ([Pop_dynamic_targeted(Tdp_unop_2 op)], Static_terminus(o1))
        | _ -> Enum.empty ()
      end;
    | Udp_binop_1 (op,x1,x2,dst,o0,o1) ->
      match element with
      | Lookup_value_variable _ ->
        Enum.singleton ([Push (Lookup_binop); Push (Lookup_jump dst); Push (Lookup_capture 3); Push(Lookup_value_variable x2); Push (Lookup_jump dst); Push (Lookup_capture 5); Push (Lookup_value_variable x1;)], Static_terminus(o0))
      | Lookup_binop ->
        Enum.singleton ([Pop_dynamic_targeted(Tdp_binop_2 op)], Static_terminus(o1))
      | _ -> Enum.empty ()
  ;;
end;;
