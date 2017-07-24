open Batteries;;
open Jhupllib;;
open Nondeterminism;;

open Analysis_grammar;;
open Analysis_types;;
open Analysis_lookup_dph.Dph;;
open Analysis_lexical_relations;;
open Analysis_lookup_basis;;
open State;;
open Program_state;;
open Terminus.T;;
open Stack_element;;
open Stack_action.T;;
open Untargeted_dynamic_pop_action;;
open Targeted_dynamic_pop_action;;

let global_edge_function state =
  let open Nondeterminism_monad in
  [
    (* Capture n *)
    return ([], Dynamic_terminus(Udp_result));
    (* Jump *)
    return ([], Dynamic_terminus(Udp_jump));
    (* Capture n *)
    return ([Pop_dynamic_targeted(Tdp_capture_1)], Static_terminus(state));
    (* Bind *)
    return ([Pop(Lookup_bind); Pop_dynamic_targeted(Tdp_bind_1)], Static_terminus(state));
    (* Project *)
    return ([Pop(Lookup_project); Pop_dynamic_targeted(Tdp_project_1)], Static_terminus(state));
    (* Index *)
    return ([Pop(Lookup_index); Pop_dynamic_targeted(Tdp_index_1)], Static_terminus(state));
    (* Is *)
    return ([Pop(Lookup_is); Pop_dynamic_targeted(Tdp_is_1)], Static_terminus(state));
  ]
  |> List.enum
  |> Enum.map Nondeterminism_monad.enum
  |> Enum.concat
;;

let per_cfg_edge_function rmr src dst state =
  (* ignore rmr; *)
  if State.equal state (Program_state dst) then
    let open Nondeterminism_monad in
    let o0 = Program_state(dst) in
    let o1 = Program_state(src) in
    [
      (* Let x = e *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Let_expression (x,e)))) = o1 in
        match e with
        | Integer_literal sgn ->
          return ([Pop (Lookup_value_variable x); Push (Lookup_value (Integer_value sgn))], Static_terminus(o1))
        | String_literal str ->
          return ([Pop (Lookup_value_variable x); Push (Lookup_value (String_value str))], Static_terminus(o1))
        | Boolean_literal b ->
          return ([Pop (Lookup_value_variable x); Push (Lookup_value (Boolean_value b))], Static_terminus(o1))
        | List_expression _ -> raise @@ Utils.Not_yet_implemented "per_cfg_edge_function: list_value"
        | Function_expression (args, block) ->
          return ([Pop (Lookup_value_variable x); Push (Lookup_value (Function_value (args, block)))], Static_terminus(o1))
        | None_literal ->
          return ([Pop (Lookup_value_variable x); Push (Lookup_value (None_value))], Static_terminus(o1))
        | Empty_binding ->
          return ([Pop (Lookup_value_variable x); Push (Lookup_value (Object_value AbstractStringMap.empty))], Static_terminus(o1))

      end;
      (* Let y = alloc *)
      begin
        let%orzero Program_state (Stmt (Statement(uid, Let_alloc y))) = o1 in
        return ([Pop (Lookup_memory_variable y); Push (Lookup_memory (Memloc (Statement(uid, Let_alloc y))))], Static_terminus(o1))
      end;
      (* Let x1 = x2 *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Let_alias_value (x1, x2)))) = o1 in
        return ([Pop (Lookup_value_variable x1); Push (Lookup_value_variable x2)], Static_terminus(o1))
      end;
      (* Let y1 = y2 *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Let_alias_memory (y1, y2)))) = o1 in
        return ([Pop (Lookup_memory_variable y1); Push (Lookup_memory_variable y2)], Static_terminus(o1))
      end;
      (* Let b = b'{x->y} *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Let_binding_update (b, b', x, y)))) = o1 in
        return ([Pop (Lookup_value_variable b); Push (Lookup_bind); Push (Lookup_jump dst); Push (Lookup_capture 3); Push(Lookup_memory_variable y); Push (Lookup_jump dst); Push (Lookup_capture 5); Push(Lookup_value_variable x); Push (Lookup_jump dst); Push (Lookup_capture 7); Push(Lookup_value_variable b')], Static_terminus(o1))
      end;
      (* Let y = b{x} *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Let_binding_access (y,b,x)))) = o1 in
        return ([Pop (Lookup_memory_variable y); Push (Lookup_project); Push (Lookup_jump dst); Push (Lookup_capture 3); Push(Lookup_value_variable x); Push (Lookup_jump dst); Push (Lookup_capture 5); Push(Lookup_value_variable b)], Static_terminus(o1))
      end;
      (* Let y = lst[i] *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Let_list_access (y,lst,i)))) = o1 in
        return ([Pop (Lookup_memory_variable y); Push (Lookup_index); Push (Lookup_jump dst); Push (Lookup_capture 3); Push(Lookup_value_variable i); Push (Lookup_jump dst); Push (Lookup_capture 5); Push(Lookup_value_variable lst)], Static_terminus(o1))
      end;
      (* Let y = lst[m:n] *)
      (* TODO: list slicing *)
      (* Store y x *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Store (y,_)))) = o1 in
        return ([Pop_dynamic_targeted(Tdp_store(y,dst))], Static_terminus(o1))
        (* return [Push (Lookup_isalias); Push (Lookup_jump dst); Push (Lookup_capture 2); Push(Lookup_memory_variable y)] *)
      end;
      (* Let x = get y *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Let_get (x,y)))) = o1 in
        return ([Pop (Lookup_value_variable x); Push (Lookup_dereference); Push (Lookup_jump dst); Push (Lookup_capture 1); Push(Lookup_memory_variable y)], Static_terminus(o1))
      end;
      (* Let x = y1 is y2 *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Let_is (x,y1,y2)))) = o1 in
        return ([Pop (Lookup_value_variable x); Push (Lookup_is); Push (Lookup_jump dst); Push (Lookup_capture 3); Push(Lookup_memory_variable y2); Push (Lookup_jump dst); Push (Lookup_capture 5); Push(Lookup_memory_variable y1)], Static_terminus(o1))
      end;
      (* Let x = unop x' *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Let_unop (_,op,x')))) = o1 in
        return ([Pop_dynamic_targeted(Tdp_unop_1(op,x',dst))], Static_terminus(o0))
        (* return ([Pop (Lookup_value_variable x); Push (Lookup_unop); Push (Lookup_jump dst); Push (Lookup_capture 3); Push(Lookup_value_variable x')], Static_terminus(o0)) *)
      end;
      (* Let x = x1 binop x2 *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Let_binop (_,x1,op,x2)))) = o1 in
        return ([Pop_dynamic_targeted(Tdp_binop_1(op,x1,x2,dst))], Static_terminus(o0))
        (* return ([Pop (Lookup_value_variable x); Push (Lookup_binop); Push (Lookup_jump dst); Push (Lookup_capture 3); Push(Lookup_value_variable x2); Push (Lookup_jump dst); Push (Lookup_capture 5); Push(Lookup_value_variable x1)], Static_terminus(o0)) *)
      end;

      (* Skip While Loop *)
      begin
        let%orzero Program_state (Stmt s) = o1 in
        let%orzero Statement(_, While (_,_)) = s in
        let%orzero Program_state (Advance s') = o0 in
        let%orzero Statement(_, While (_,_)) = s' in
        return ([Push (Lookup_jump (Stmt s))], Static_terminus(o1))
      end;
      (* TODO: check if left loop  *)
      (* While top x *)
      begin
        let%orzero Program_state (Stmt (s)) = o1 in
        let%orzero Statement(_, While (_,_)) = s in
        let%orzero Program_state (Stmt _) = o0 in
        return ([Pop_dynamic_targeted(Tdp_peek_x None)], Static_terminus(o1))
      end;
      (* While top y *)
      begin
        let%orzero Program_state (Stmt (s)) = o1 in
        let%orzero Statement(_, While (_,_)) = s in
        let%orzero Program_state (Stmt _) = o0 in
        return ([Pop_dynamic_targeted(Tdp_peek_y None)], Static_terminus(o1))
      end;
      (* Ifresult x *)
      begin
        let%orzero Program_state (Ifresult _) = o1 in
        let%orzero Advance (s) = dst in
        let%orzero Statement(_, Let_conditional_value (x,_,_,_)) = s in
        return ([], Dynamic_terminus(Udp_ifresult_x (x,o1,Stmt(s))))
      end;
      (* Ifresult y *)
      begin
        let%orzero Program_state (Ifresult _) = o1 in
        let%orzero Advance (s) = dst in
        let%orzero Statement(_, Let_conditional_memory (y,_,_,_)) = s in
        return ([], Dynamic_terminus(Udp_ifresult_y (y,o1,Stmt(s))))
      end;
      (* Function search *)
      begin
        let%orzero Program_state (Stmt (s)) = o1 in
        let%orzero Statement(_, Let_call_function (_,_,lst)) = s in
        let%orzero Some Statement(_, Let_expression (x, Function_expression (lst',_))) = (Stmt_map.find s rmr.down) in
        if List.length lst = List.length lst' then
          return ([Pop_dynamic_targeted(Tdp_func_search (x,lst'))], Static_terminus(o1))
        else
          return ([], Static_terminus(o0)) (* FIXME *)
         end;
      (* Function return *)
      begin
        let%orzero Program_state (Return _) = o1 in
        let%orzero Advance (s) = dst in
        let%orzero Statement(_, Let_call_function (y,_,_)) = s in
        return ([], Dynamic_terminus(Udp_return (y,o1,Stmt(s))))
      end;
      (* Try/Except top x *)
      begin
        let%orzero Program_state (Stmt (s)) = o1 in
        let%orzero Statement(_, Try_except (_,_,_)) = s in
        let%orzero Program_state (Stmt _) = o0 in
        return ([Pop_dynamic_targeted(Tdp_peek_x None)], Static_terminus(o1))
      end;
      (* Try/Except top y *)
      begin
        let%orzero Program_state (Stmt (s)) = o1 in
        let%orzero Statement(_, Try_except (_,_,_)) = s in
        let%orzero Program_state (Stmt _) = o0 in
        return ([Pop_dynamic_targeted(Tdp_peek_y None)], Static_terminus(o1))
      end;
      (* Raise *)
      begin
        let%orzero Program_state (Raise _) = o1 in
        let%orzero Advance (s) = dst in
        let%orzero Statement(_, Try_except (_,y,_)) = s in
        return ([], Dynamic_terminus(Udp_raise (y,o1,Stmt(s))))
      end;

      (* Is Alias *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Store (_,x)))) = o1 in
        return ([Pop Lookup_isalias; Pop_dynamic_targeted(Tdp_isalias_1 x)], Static_terminus(o1))
      end;
      (* Trace and get Answer *)
      begin
        match src with
        | Program_state.Return s
        | Program_state.Ifresult s
        | Program_state.Raise s -> return ([Pop(Lookup_answer); Push(Lookup_answer)], Static_terminus(Program_state (Stmt s)))
        | Stmt (Statement(_, Analysis_types.Return y))
        | Stmt (Statement(_, If_result_memory y))
        | Stmt (Statement(_, Analysis_types.Raise y)) -> return ([Pop(Lookup_answer); Push(Lookup_memory_variable y)], Static_terminus o1)
        | _ -> return ([], Static_terminus(o0)) (* TODO: change this *)
      end;

      (* Skip x with y *)
      begin
        let%orzero (
          Stmt (Statement(_, Let_expression _))
        | Stmt (Statement(_, Let_alias_value _))
        | Stmt (Statement(_, Let_binding_update _))
        | Stmt (Statement(_, Let_list_slice _))
        | Stmt (Statement(_, Let_get _))
        | Stmt (Statement(_, Let_is _))
        | Stmt (Statement(_, Let_unop _))
        | Stmt (Statement(_, Let_binop _))
        )
          = src in
        return ([Pop_dynamic_targeted(Tdp_peek_y None)], Static_terminus(o1))
      end;
      (* Skip y with x *)
      begin
        let%orzero (
          Stmt (Statement(_, Let_alloc _))
        | Stmt (Statement(_, Let_alias_memory _))
        | Stmt (Statement(_, Let_binding_access _))
        | Stmt (Statement(_, Let_list_access _))
        )
          = src in
        return ([Pop_dynamic_targeted(Tdp_peek_x None)], Static_terminus(o1))
      end;
      (* Skip x with x' *)
      begin
        let%orzero (
          Stmt (Statement(_, Let_expression (x,_)))
        | Stmt (Statement(_, Let_alias_value (x,_)))
        | Stmt (Statement(_, Let_binding_update (x,_,_,_)))
        | Stmt (Statement(_, Let_list_slice (x,_,_,_)))
        | Stmt (Statement(_, Let_get (x,_)))
        | Stmt (Statement(_, Let_is (x,_,_)))
        | Stmt (Statement(_, Let_unop (x,_,_)))
        | Stmt (Statement(_, Let_binop (x,_,_,_)))
        )
          = src in
        return ([Pop_dynamic_targeted(Tdp_peek_x (Some x))], Static_terminus(o1))
      end;
      (* Skip y with y' *)
      begin
        let%orzero (
          Stmt (Statement(_, Let_alloc y))
        | Stmt (Statement(_, Let_alias_memory (y,_)))
        | Stmt (Statement(_, Let_binding_access (y,_,_)))
        | Stmt (Statement(_, Let_list_access (y,_,_)))
        )
          = src in
        return ([Pop_dynamic_targeted(Tdp_peek_y (Some y))], Static_terminus(o1))
      end;
      (* Skip non-Store with m! *)
      begin
        match o1 with
        | Program_state (Stmt ((Statement(_, Store _)))) -> return ([Pop_dynamic_targeted(Tdp_peek_m_1)], Static_terminus(o0))
        | _ ->return ([Pop_dynamic_targeted(Tdp_peek_m_1)], Static_terminus(o1))
      end;
      (* Skip Try/Except, follow advance otherwise *)
      begin
        let%orzero Program_state (Advance target) = o0 in
        return ([], (Dynamic_terminus(Udp_advance (target,o1))))
      end;
    ]
    |> List.enum
    |> Enum.map Nondeterminism_monad.enum
    |> Enum.concat
  else
    Enum.empty ()
;;