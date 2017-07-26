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

let logger = make_logger "Analysis_lookup_dph";;
set_default_logging_level `debug;;

let print_value v =
  match v with
  | Integer_value n ->
    begin
      match n with
      | Pos -> "int+"
      | Neg -> "int-"
      | Zero -> "int0"
    end
  | String_value s ->
    begin
      match s with
      | String_exact s -> s
      | String_lossy -> "blah"
    end
  | Boolean_value b -> string_of_bool b
  | None_value -> "None"
  | Object_value _ -> "object"
  | List_value _ -> "list"
  | Function_value _ -> "function"
;;

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
      | Tdp_capture_v_1
      | Tdp_capture_v_2 of value
      | Tdp_capture_v_3 of value * int * Stack_element.t list
      | Tdp_capture_m_1
      | Tdp_capture_m_2 of memory_location
      | Tdp_capture_m_3 of memory_location * int * Stack_element.t list
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
      | Tdp_unop_1 of value_variable * unary_operator * value_variable * Program_state.t
      | Tdp_unop_2 of unary_operator
      | Tdp_binop_1 of value_variable * binary_operator * value_variable * value_variable * Program_state.t
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
      | Udp_advance_while of bool * State.t
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
      (* Capture steps v *)
      begin
        let%orzero Tdp_capture_v_1 = action in
        let%orzero Lookup_value v = element in
        (* let str = print_value v in *)
        (* let () = logger `debug ("capture step 1: "^str) in *)
        return [Pop_dynamic_targeted (Tdp_capture_v_2 v)]
      end;
      begin
        let%orzero Tdp_capture_v_2 v = action in
        let%orzero Lookup_capture n = element in
        (* let () = logger `debug "capture step 2" in *)
        return [Pop_dynamic_targeted (Tdp_capture_v_3 (v,n,[]))]
      end;
      begin
        let%orzero Tdp_capture_v_3 (v,n,lst) = action in
        if n > 1 then
          (* let () = logger `debug ("capture "^(string_of_int n)) in *)
          return [Pop_dynamic_targeted (Tdp_capture_v_3 (v,n-1,element::lst))]
        else
          return @@ [Push (Lookup_value v); Push element] @ List.map (fun x -> Push x) (lst)
          (* return @@ [Push element; Push (Lookup_value v)] @ List.map (fun x -> Push x) (lst) *)
      end;

      (* Capture steps m *)
      begin
        let%orzero Tdp_capture_m_1 = action in
        (* let () = logger `debug "capture step 1" in *)
        let%orzero Lookup_memory m = element in
        return [Pop_dynamic_targeted (Tdp_capture_m_2 m)]
      end;
      begin
        let%orzero Tdp_capture_m_2 m = action in
        let%orzero Lookup_capture n = element in
        (* let () = logger `debug "capture step 2" in *)
        return [Pop_dynamic_targeted (Tdp_capture_m_3 (m,n,[]))]
      end;
      begin
        let%orzero Tdp_capture_m_3 (m,n,lst) = action in
        if n > 1 then
          (* let () = logger `debug ("capture "^(string_of_int n)) in *)
          return [Pop_dynamic_targeted (Tdp_capture_m_3 (m,n-1,element::lst))]
        else
          return @@ [Push (Lookup_memory m); Push element] @ List.map (fun x -> Push x) (lst)
          (* return @@ [Push element; Push (Lookup_memory m)] @ List.map (fun x -> Push x) (lst) *)
      end;

      (* Bind steps *)
      begin
        let%orzero Tdp_bind_1 = action in
        let () = logger `debug "bind step 1" in
        let%orzero Lookup_value(Object_value v) = element in
        return [Pop_dynamic_targeted (Tdp_bind_2 v)]
      end;
      begin
        let%orzero Tdp_bind_2 v = action in
        let%orzero Lookup_value(String_value str) = element in
        let () = logger `debug "bind step 2" in
        return [Pop_dynamic_targeted (Tdp_bind_3 (v,str))]
      end;
      begin
        let%orzero Tdp_bind_3 (v,str) = action in
        let%orzero Lookup_memory m = element in
        let () = logger `debug "bind step 3" in
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
        let () = logger `debug "store" in
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
          return [Push (element)]
        | Some Value_variable id' ->
          [%guard id <> id'];
          return [Push (element)]
      end;
      begin
        let%orzero Tdp_peek_y y = action in
        let%orzero Lookup_memory_variable (Memory_variable id) = element in
        match y with
        | None ->
          return [Push (element)]
        | Some Memory_variable id' ->
          [%guard id <> id'];
          return [Push (element)]
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
        let () = logger `debug "isalias step 1" in
        return [Pop_dynamic_targeted(Tdp_isalias_2 (x,m))]
      end;
      begin
        let%orzero Tdp_isalias_2 (x,m) = action in
        let%orzero Lookup_memory m' = element in
        if equal_memory_location m m' then
          let () = logger `debug "isalias step 2: true" in
          return [Pop(Lookup_dereference); Push(Lookup_value_variable x)]
        else
          let () = logger `debug "isalias step 2: false" in
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

      (* Unop steps*)
      begin
        let%orzero Tdp_unop_1 (x,op,x',dst) = action in
        (* return [Push (Lookup_unop); Push (Lookup_jump dst); Push (Lookup_capture 3); Push(Lookup_value_variable x')] *)
        match element with
        | Lookup_value_variable id ->
          [%guard equal_value_variable id x];
          return [Push (Lookup_unop); Push (Lookup_jump dst); Push (Lookup_capture 2); Push(Lookup_value_variable x')]
        | Lookup_unop ->
          return [Pop_dynamic_targeted(Tdp_unop_2 op)]
        | _ -> return []
      end;
      begin
        let%orzero Tdp_unop_2 op = action in
        let%orzero Lookup_value v = element in
        let () = logger `debug "unop step 2" in
        let%bind v' = pick_enum(unary_operation op v) in
        return [Push(Lookup_value v')]
      end;

      (* Binop steps *)
      begin
        let%orzero Tdp_binop_1 (x,op,x1,x2,dst) = action in
        match element with
        | Lookup_value_variable id ->
          [%guard equal_value_variable id x];
          return [Push (Lookup_binop); Push (Lookup_jump dst); Push (Lookup_capture 3); Push(Lookup_value_variable x2); Push (Lookup_jump dst); Push (Lookup_capture 5); Push (Lookup_value_variable x1;)]
        | Lookup_binop ->
          return [Pop_dynamic_targeted(Tdp_binop_2 op)]
        | _ -> return []
      end;
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
    (* let open Nondeterminism_monad in *)
    match action with
    | Udp_result ->
      begin
        match element with
        | Lookup_value v ->
          let () = logger `debug ("value state: " ^ print_value v) in
          Enum.singleton ([Pop(Bottom)], Static_terminus(Answer_value v))
        | _ -> Enum.empty ()
      end
    | Udp_jump ->
      begin
        match element with
        | Lookup_jump state ->
          let () = logger `debug "jump" in
          Enum.singleton ([], Static_terminus(Program_state state))
        | _ -> Enum.empty ()
      end
    | Udp_ifresult_x (x,prev,skip) ->
      begin
        match element with
        | Lookup_value_variable x' ->
          if equal_value_variable x x' then
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
          if equal_memory_variable y y' then
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
        | Statement(_, Try_except _)
        | Statement(_, Let_call_function _) ->
          begin
            match element with
            | Lookup_memory _ ->
              Enum.singleton ([Push (element)], Static_terminus prev)
            | _->
              let () = logger `debug "skip try/except" in
              Enum.singleton ([Push (element)], Static_terminus(Program_state (Stmt target)))
          end
        (* | Statement(_, While (_,_)) ->
           Enum.singleton ([Push (element)], Static_terminus (Program_state (Stmt target))) *)
        | _ ->
          Enum.singleton ([Push (element)], Static_terminus prev)
          (* begin
                      match element with
                      | Lookup_memory _ ->
                        Enum.singleton ([Push (element)], Static_terminus prev)
                      | _ ->
                        (* If advance does not point to a statement "under" the current statement---not in a while loop, then go to advance. Otherwise, don't proceed in that universe. *)
                        if not is_parent then
                          Enum.singleton ([Push (element)], Static_terminus prev)
                        else
                          Enum.empty ()

                    end *)
      end
    | Udp_advance_while (is_parent, prev)->
      begin
        match element with
        | Lookup_memory _ ->
          Enum.singleton ([Push (element)], Static_terminus prev)
        | _ ->
          (* If advance does not point to a statement "under" the current statement---not in a while loop, then go to advance. Otherwise, don't proceed in that universe. *)
          if is_parent then
            let () = logger `debug "is_parent" in
            Enum.singleton ([Pop (Lookup_value_variable (Value_variable "impossible"))], Static_terminus prev)
          else
            let () = logger `debug "not_parent" in
            Enum.singleton ([Push (element)], Static_terminus prev)


      end
  ;;
end;;
