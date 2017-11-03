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
      | Tdp_list_1
      | Tdp_list_2 of int * memory_location list
      | Tdp_index_1
      | Tdp_index_2 of abstract_memloc_list
      | Tdp_slice_1
      | Tdp_slice_2 of abstract_memloc_list
      | Tdp_slice_3 of abstract_memloc_list * sign
      | Tdp_store of memory_variable * Program_state.t
      | Tdp_isalias_1 of value_variable
      | Tdp_isalias_2 of value_variable * memory_location
      | Tdp_is_1
      | Tdp_is_2 of memory_location
      | Tdp_func_search of value_variable * value_variable list * value_variable list
      | Tdp_unop_1 of value_variable * unary_operator * value_variable * Program_state.t
      | Tdp_unop_2 of unary_operator
      | Tdp_binop_1 of value_variable * binary_operator * value_variable * value_variable * Program_state.t * Program_state.t
      | Tdp_binop_2 of binary_operator
      | Tdp_binop_3 of binary_operator * value
      | Tdp_trace_x of value_variable
      | Tdp_trace_y of memory_variable
      | Tdp_drop
      | Tdp_raise
    [@@deriving eq, ord, show, to_yojson]
    ;;
  end;;
  module Untargeted_dynamic_pop_action =
  struct
    type t =
      | Udp_result
      | Udp_jump
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
        return [Pop_dynamic_targeted (Tdp_capture_v_2 v)]
      end;
      begin
        let%orzero Tdp_capture_v_2 v = action in
        let%orzero Lookup_capture n = element in
        return [Pop_dynamic_targeted (Tdp_capture_v_3 (v,n,[]))]
      end;
      begin
        let%orzero Tdp_capture_v_3 (v,n,lst) = action in
        if n > 1 then
          return [Pop_dynamic_targeted (Tdp_capture_v_3 (v,n-1,element::lst))]
        else
          return @@ [Push (Lookup_value v); Push element] @ List.map (fun x -> Push x) (lst)
      end;

      (* Capture steps m *)
      begin
        let%orzero Tdp_capture_m_1 = action in
        let%orzero Lookup_memory m = element in
        return [Pop_dynamic_targeted (Tdp_capture_m_2 m)]
      end;
      begin
        let%orzero Tdp_capture_m_2 m = action in
        let%orzero Lookup_capture n = element in
        return [Pop_dynamic_targeted (Tdp_capture_m_3 (m,n,[]))]
      end;
      begin
        let%orzero Tdp_capture_m_3 (m,n,lst) = action in
        if n > 1 then
          return [Pop_dynamic_targeted (Tdp_capture_m_3 (m,n-1,element::lst))]
        else
          return @@ [Push (Lookup_memory m); Push element] @ List.map (fun x -> Push x) (lst)
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

      (* List steps *)
      begin
        let%orzero Tdp_list_1 = action in
        let%orzero Lookup_list n = element in
        return [Pop_dynamic_targeted (Tdp_list_2 (n,[]))]
      end;
      begin
        let%orzero Tdp_list_2 (n,lst) = action in
        let%orzero Lookup_memory m = element in
        if n > 1 then
          return [Pop_dynamic_targeted (Tdp_list_2 (n-1,m::lst))]
        else
          return @@ [Push (Lookup_value (List_value (List_exact (lst,List.length lst))))]
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
        | Zero ->
          begin
            match lst with
            | List_exact (le,_) ->
              let m = List.hd le in
              return [Push(Lookup_memory m)]
            | List_lossy ll ->
              let m = List.hd ll in
              return [Push(Lookup_memory m)]
          end
        | Pos ->
          begin
            match lst with
            | List_exact (le,_) ->
              let%bind m = pick_enum (List.enum le) in
              return [Push(Lookup_memory m)]
            | List_lossy ll ->
              let%bind m = pick_enum (List.enum ll) in
              return [Push(Lookup_memory m)]
          end
      end;

      (* Slice steps *)
      (* Note: in real life, only the first tdp is in use *)
      (* begin
        let%orzero Tdp_slice_1 = action in
        let%orzero Lookup_value(List_value lst) = element in
        return [Pop_dynamic_targeted (Tdp_index_2 lst)]
         end; *)
      begin
        let%orzero Tdp_slice_1 = action in
        let%orzero Lookup_value(List_value lst) = element in
        match lst with
        | List_exact (le,_) ->
          return [Push(Lookup_value (List_value (List_lossy le)))]
        | List_lossy _ ->
          return [Push(Lookup_value (List_value lst))]
      end;

      (* Obsolete from here *)
      begin
        let%orzero Tdp_slice_2 lst = action in
        let%orzero Lookup_value(Integer_value v1) = element in
        return [Pop_dynamic_targeted (Tdp_slice_3 (lst,v1))]
      end;
      begin
        let%orzero Tdp_slice_3 (lst,v1) = action in
        let%orzero Lookup_value(Integer_value v2) = element in
        (* [%guard (equal_sign v1 Pos && equal_sign v2 Pos)
                || (equal_sign v1 Zero && equal_sign v2 Pos)
                || (equal_sign v1 Zero && equal_sign v2 Zero)]; *)
        match lst with
        | List_exact (le,n) ->
          begin
            match v1,v2 with
            | Pos,Pos ->
              let bound = List.length le in
              let range = List.of_enum (0--(bound-1))in
              let%bind i = pick_enum (List.enum range) in
              let%bind j = pick_enum (List.enum range) in
              let slice =
                if i <= j then List.enum(le) |> Enum.skip i |> Enum.take (j+1-i) else List.enum(le) |> Enum.skip j |> Enum.take (i+1-j)
              in
              return [Push (Lookup_value(List_value (List_exact(List.of_enum slice, n))))]
            | Zero,Zero ->
              return [Push (Lookup_value(List_value (List_exact([List.hd le], n))))]
            | Zero,Pos ->
              let bound = List.length le in
              let range = List.of_enum (0--(bound-1))in
              let%bind i = pick_enum (List.enum range) in
              let slice = List.take i le in
              return [Push (Lookup_value(List_value (List_exact (slice,n))))]
            | _ -> return []
          end

        | List_lossy ll ->
          let bound = List.length ll in
          let range = List.of_enum (0--(bound-1))in
          let%bind i = pick_enum (List.enum range) in
          let slice = List.take i ll in
          return [Push (Lookup_value(List_value (List_lossy slice)))] (* TODO *)
      end;

      (* Store *)
      begin
        let%orzero Tdp_store (y,o0) = action in
        let%orzero Lookup_memory _ = element in
        return [Pop(Lookup_dereference); Push (element); Push (Lookup_isalias); Push (Lookup_jump o0); Push (Lookup_capture 2); Push(Lookup_memory_variable y)]
      end;

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
        return [Pop_dynamic_targeted(Tdp_isalias_2 (x,m))]
      end;
      begin
        let%orzero Tdp_isalias_2 (x,m) = action in
        let%orzero Lookup_memory m' = element in
        if equal_memory_location m m' then
          return [Push(Lookup_value_variable x)]
        else
          return [Push(Lookup_dereference); Push(element)]
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
        let%orzero Tdp_func_search (x,lst,lst') = action in
        match element with
        | Lookup_value_variable xi ->
          begin
            match List.index_of xi lst' with
            | None ->
              return [Push (element); Push (Lookup_drop); Push(Lookup_capture 1); Push (Lookup_value_variable x)]
            | Some n ->
              return [Push(Lookup_value_variable (List.at lst n))]
          end
        | Lookup_memory _ ->
          return [Push(element)]
        | _ ->
          return [Push (element); Push (Lookup_drop); Push(Lookup_capture 1); Push (Lookup_value_variable x)]
      end;

      (* Unop steps*)
      begin
        let%orzero Tdp_unop_1 (x,op,x',dst) = action in
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
        let%bind v' = pick_enum(unary_operation op v) in
        return [Push(Lookup_value v')]
      end;

      (* Binop steps *)
      begin
        let%orzero Tdp_binop_1 (x,op,x1,x2,src,dst) = action in
        match element with
        | Lookup_value_variable id ->
          [%guard equal_value_variable id x];
          return [Push (Lookup_binop); Push (Lookup_jump dst); Push (Lookup_capture 3); Push(Lookup_value_variable x2); Push (Lookup_jump src); Push (Lookup_capture 5); Push (Lookup_value_variable x1;)]
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

      begin
        let%orzero Tdp_trace_x x = action in
        match element with
        | Lookup_answer ->
          return [Push (Lookup_value_variable x)]
        | _ ->
          return [Push (element)]
      end;

      begin
        let%orzero Tdp_trace_y y = action in
        match element with
        | Lookup_answer ->
          return [Push (Lookup_memory_variable y)]
        | _ ->
          return [Push (element)]
      end;

      begin
        let%orzero Tdp_drop = action in
        return []
      end;

      begin
        let%orzero Tdp_raise = action in
        match element with
        | Lookup_memory_variable _ ->
          return [Push (Lookup_answer)]
        | _ ->
          return [Push (element)]
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
          Enum.singleton ([Pop(Bottom)], Static_terminus(Answer_value v))
        | _ -> Enum.empty ()
      end
    | Udp_jump ->
      begin
        match element with
        | Lookup_jump state ->
          Enum.singleton ([], Static_terminus(Program_state state))
        | _ -> Enum.empty ()
      end
    | Udp_raise (_,prev,_) ->
      begin
        match element with
        | Lookup_memory _ ->
          Enum.singleton ([Push (Lookup_answer)], Static_terminus(prev))
        | _ -> Enum.empty ()
      end
    (* Udp_advance checks if we're about to enter a block, and either skips that block
       or enters it based on what we're looking up. *)
    | Udp_advance (target, prev) ->
      begin
        let skip_body = Enum.singleton ([Push (element)], Static_terminus(Program_state (Stmt target))) in
        let enter_body = Enum.singleton ([Push (element)], Static_terminus prev) in
        let enter_body_and_change_target = Enum.singleton ([Push (Lookup_answer)], Static_terminus prev) in
        match target with
        (* For while loops, we can't actually see inside the body from the
           Advance(while) node, so just step back on to the while *)
        | Statement(_, While _) ->
          Enum.singleton ([Push (element)], Static_terminus prev)
        (* For other blocks, we always enter them if we're looking up the contents of
           a memory address, since the heap could change at any time *)
        | Statement(_, Try_except _) ->
          begin
            match element with
            | Lookup_memory _ -> enter_body
            | _-> skip_body
          end
        (* If the block in question binds a variable, we also enter the block
           if we're looking for the variable that was bound; in that case, we
           start looking for the return (or ifresult, etc) value of the block *)
        | Statement(_, Let_call_function (y,_,_))
        | Statement(_, Let_conditional_memory (y,_,_,_)) ->
          begin
            match element with
            | Lookup_memory _ -> enter_body

            | Lookup_memory_variable y' ->
              if equal_memory_variable y y' then
                enter_body_and_change_target
              else
                skip_body

            | _-> skip_body
          end
        | Statement(_, Let_conditional_value (x,_,_,_)) ->
          begin
            match element with
            | Lookup_memory _ -> enter_body

            | Lookup_value_variable x' ->
              if equal_value_variable x x' then
                enter_body_and_change_target
              else
                skip_body

            | _-> skip_body

          end
        (* If the statement doesn't create a new block, then just step backwards *)
        | _ -> Enum.singleton ([Push (element)], Static_terminus prev)
      end

    | Udp_advance_while (is_parent, prev)->
      begin
        match element with
        | Lookup_memory _ ->
          Enum.singleton ([Push (element)], Static_terminus prev)
        | _ ->
          (* If advance does not point to a statement "under" the current statement
             ---not in a while loop, then go to advance. Otherwise, don't proceed in that universe. *)
          if is_parent then
            Enum.empty ()
          else
            Enum.singleton ([Push (element)], Static_terminus prev)


      end
  ;;
end;;
