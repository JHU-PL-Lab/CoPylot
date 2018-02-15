open Batteries;;
open Jhupllib;;
open Nondeterminism;;

open Logger_utils;;
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

let logger = make_logger "Analysis_lookup_edge_functions";;

let get_uid ps =
  match ps with
  | Stmt ( Statement (uid,_)) -> string_of_int uid
  | Advance ( Statement (uid,_)) -> "adv " ^ (string_of_int uid)
  | Return ( Statement (uid,_)) -> "ret " ^ (string_of_int uid)
  | Ifresult ( Statement (uid,_)) -> "ifr " ^ (string_of_int uid)
  | Raise ( Statement (uid,_)) -> "rai " ^ (string_of_int uid)
  | Start -> "start"
  | End -> "end"
;;

let log_debug src dst str =
  let s1 = get_uid src in
  let s0 = get_uid dst in
  logger `debug (s1 ^ " -> " ^ s0 ^ ": " ^ str)
;;

let global_edge_function state =

  let open Nondeterminism_monad in
  [
    (* Result *)
    return ([], Dynamic_terminus(Udp_result));
    (* Jump *)
    return ([], Dynamic_terminus(Udp_jump));
    (* Drop *)
    return ([Pop Lookup_drop; Pop_dynamic_targeted(Tdp_drop)], Static_terminus(state));
    (* Capture n *)
    return ([Pop_dynamic_targeted(Tdp_capture_v_1)], Static_terminus(state));
    return ([Pop_dynamic_targeted(Tdp_capture_m_1)], Static_terminus(state));
    (* Bind *)
    return ([Pop(Lookup_bind); Pop_dynamic_targeted(Tdp_bind_1)], Static_terminus(state));
    (* Project *)
    return ([Pop(Lookup_project); Pop_dynamic_targeted(Tdp_project_1)], Static_terminus(state));
    (* List n *)
    return ([Pop_dynamic_targeted(Tdp_list_1)], Static_terminus(state));
    (* Index *)
    return ([Pop(Lookup_index); Pop_dynamic_targeted(Tdp_index_1)], Static_terminus(state));
    (* Slice *)
    return ([Pop(Lookup_slice); Pop_dynamic_targeted(Tdp_slice_1)], Static_terminus(state));
    (* Is *)
    return ([Pop(Lookup_is); Pop_dynamic_targeted(Tdp_is_1)], Static_terminus(state));

  ]
  |> List.enum
  |> Enum.map Nondeterminism_monad.enum
  |> Enum.concat
;;

let per_cfg_edge_function rmr src dst state =
  if State.equal state (Program_state dst) then
    let open Nondeterminism_monad in
    (* log_debug src dst @@ "Begin"; *)
    let o0 = Program_state(dst) in
    let o1 = Program_state(src) in
    [
      (* Let x = e *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Let_expression (x,e)))) = o1 in
        match e with
        | Integer_literal num ->
          (* let () = log_debug src dst "let x = int" in *)
          return ([Pop (Lookup_value_variable x); Push (Lookup_value (Integer_value num))], Static_terminus(o1))
        | String_literal str ->
          (* let () = log_debug src dst @@ "let x = str: " ^ show_abstract_str str in *)
          return ([Pop (Lookup_value_variable x); Push (Lookup_value (String_value str))], Static_terminus(o1))
        | Boolean_literal b ->
          return ([Pop (Lookup_value_variable x); Push (Lookup_value (Boolean_value b))], Static_terminus(o1))
        | List_expression lst ->
          let n = List.length lst in
          (* let () = log_debug src dst (String.concat "" (List.map (fun j -> Yojson.Safe.to_string j) (List.map memory_variable_to_yojson lst))) in *)
          let%orzero Program_state (tgt) = o1 in
          if n == 0 then
            return ([Pop (Lookup_value_variable x); Push (Lookup_value (List_value (List_exact ([],0))))], Static_terminus(o1))
          else
            let range = List.of_enum(1--n) in
            let () = logger `debug (string_of_int n) in
            return
              ([Pop (Lookup_value_variable x); Push (Lookup_list n)] @ List.concat @@
               List.map2
                 (fun y i ->
                   [Push (Lookup_jump tgt);
                    Push (Lookup_capture (3*i-1));
                    Push (Lookup_memory_variable y)])
                 lst
                 range, Static_terminus(o1))

        (* raise @@ Utils.Not_yet_implemented "per_cfg_edge_function: list_value" *)
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
        (* let () = logger `debug "update binding" in *)
        return ([Pop (Lookup_value_variable b); Push (Lookup_bind); Push (Lookup_jump dst); Push (Lookup_capture 4); Push(Lookup_memory_variable y); Push (Lookup_jump dst); Push (Lookup_capture 6); Push(Lookup_value_variable x); Push (Lookup_jump dst); Push (Lookup_capture 8); Push(Lookup_value_variable b')], Static_terminus(o1))
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
      begin
        let%orzero Program_state (Stmt (Statement(_, Let_list_slice (x,lst,m,n)))) = o1 in
        return ([Pop (Lookup_value_variable x); Push (Lookup_slice); Push (Lookup_jump dst); Push (Lookup_capture 4); Push(Lookup_value_variable n); Push (Lookup_jump dst); Push (Lookup_capture 6); Push(Lookup_value_variable m); Push (Lookup_jump dst); Push (Lookup_capture 8); Push(Lookup_value_variable lst)], Static_terminus(o1))
      end;
      (* Store y x *)
      begin
        (* let () = log_debug src dst "store" in *)
        let%orzero Program_state (Stmt (Statement(_, Store (y,_)))) = o1 in
        return ([Pop_dynamic_targeted(Tdp_store(y,dst))], Static_terminus(o1))
      end;
      (* Let x = get y *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Let_get (x,y)))) = o1 in
        (* let () = log_debug src dst "get" in *)
        return ([Pop (Lookup_value_variable x); Push (Lookup_dereference); Push (Lookup_jump dst); Push (Lookup_capture 1); Push(Lookup_memory_variable y)], Static_terminus(o1))
      end;
      (* Let x = y1 is y2 *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Let_is (x,y1,y2)))) = o1 in
        (* let () = log_debug src dst "let is" in *)
        return ([Pop (Lookup_value_variable x); Push (Lookup_is); Push (Lookup_jump dst); Push (Lookup_capture 3); Push(Lookup_memory_variable y2); Push (Lookup_jump dst); Push (Lookup_capture 5); Push(Lookup_memory_variable y1)], Static_terminus(o1))
      end;
      (* Let x = unop x' *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Let_unop (x,op,x')))) = o1 in
        return ([Pop_dynamic_targeted(Tdp_unop_1 (x,op,x',dst))], Static_terminus(o1))
      end;
      (* Let x = x1 binop x2 *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Let_binop (x,x1,op,x2)))) = o1 in
        return ([Pop_dynamic_targeted(Tdp_binop_1 (x,op,x1,x2,src,dst))], Static_terminus(o1))
      end;

      (* Function search *)
      begin
        let%orzero Program_state (Stmt (s)) = o1 in
        let%orzero Statement(_, Let_call_function (_,x,lst)) = s in
        let%orzero Program_state (Stmt (s')) = o0 in
        let%orzero Some Statement(_, Let_expression (_, Function_expression (lst',_))) = (Stmt_map.find s' rmr.down) in
        [%guard List.length lst = List.length lst']; (* Should never fail, but technically needed to match rule *)
        (* log_debug src dst "Function search"; *)
        return ([Pop_dynamic_targeted(Tdp_func_search (x,lst,lst'))], Static_terminus(o1))
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

      (* Is Alias *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Store (_,x)))) = o1 in
        (* let () = log_debug src dst "is alias" in *)
        return ([Pop Lookup_isalias; Pop_dynamic_targeted(Tdp_isalias_1 x)], Static_terminus(o1))
      end;

      (* Trace and get Answer (value) *)
      begin
        let%orzero Stmt (Statement(_, If_result_value x)) = src in
        (* let () = log_debug src dst "Trace end (value)" in *)
        return ([Pop_dynamic_targeted(Tdp_trace_x x)], Static_terminus o1)
      end;

      (* Trace and get Answer (memory) *)
      begin
        let%orzero (
          Stmt (Statement(_, Analysis_types.Return y))
        | Stmt (Statement(_, If_result_memory y))
        | Stmt (Statement(_, Analysis_types.Raise y)))
          = src in
        (* let () = log_debug src dst "Trace end (memory)" in *)
        return ([Pop_dynamic_targeted(Tdp_trace_y y)], Static_terminus o1)
      end;

      (* Trace exception begins *)
      begin
        let%orzero Raise _ = src in
        let%orzero Stmt _ = dst in
        (* let () = log_debug src dst "start Raise" in *)
        return ([Pop_dynamic_targeted (Tdp_raise)], Static_terminus o1)
      end;

      (* Go to block start (while, if, try) *)
      begin
        let%orzero Program_state (Stmt (Statement(_, d))) = o1 in
        let%orzero (
          While _
        | Let_conditional_value _
        | Let_conditional_memory _
        | Try_except _)
          = d in
        (* let () = log_debug src dst "go to while/if/try" in *)
        return ([Nop], Static_terminus(o1))
      end;

      (* Go to block end (return, ifresult) *)
      begin
        let%orzero (
          Program_state.Return _
        | Program_state.Ifresult _
        | Program_state.Raise _)
          = src in
        (* let () = log_debug src dst "go to return/ifresult/raise" in *)
        return ([Nop], Static_terminus(o1))
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
        | Stmt (Statement(_, Store _))
        )
          = src in
        return ([Pop_dynamic_targeted(Tdp_peek_y None)], Static_terminus(o1))
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

      (* Skip y with x *)
      begin
        let%orzero (
          Stmt (Statement(_, Let_alloc _))
        | Stmt (Statement(_, Let_alias_memory _))
        | Stmt (Statement(_, Let_binding_access _))
        | Stmt (Statement(_, Let_list_access _))
        | Stmt (Statement(_, Store _))
        )
          = src in
        return ([Pop_dynamic_targeted(Tdp_peek_x None)], Static_terminus(o1))
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
        | _ -> return ([Pop_dynamic_targeted(Tdp_peek_m_1)], Static_terminus(o1))
      end;

      (* Follow advance, unless we're standing on a while loop *)
      begin
        let%orzero Program_state (Advance target) = o1 in
        match dst with
        | Stmt (Statement (_, While _)) ->
          zero ()
        | _ ->
          return ([], (Dynamic_terminus(Udp_advance (target,o1))))
      end;

      (* Skip advance from while *)
      begin
        let%orzero Program_state (Advance target) = o1 in
        let%orzero Program_state(Stmt s) = o0 in
        let%orzero Statement(_, While _) = s in
        let target_parent = Stmt_map.find target rmr.down in
        let is_parent =
          match target_parent with
          | None -> false
          | Some tp -> equal_statement s tp in
        (* let () = log_debug src dst ("is_parent: " ^ string_of_bool is_parent) in *)
        return ([], Dynamic_terminus(Udp_advance_while (is_parent, o1)))
      end;

      (* Skip advance from while *)
      begin
        let%orzero Program_state (Advance target) = o1 in
        let%orzero Program_state(Stmt s) = o0 in
        let%orzero Statement(_, While _) = s in
        let target_parent = Stmt_map.find target rmr.down in
        let is_parent =
          match target_parent with
          | None -> false
          | Some tp -> equal_statement s tp in
        (* let () = log_debug src dst ("is_parent: " ^ string_of_bool is_parent) in *)
        return ([], Dynamic_terminus(Udp_advance_while (is_parent, o1)))
      end;
    ]
    |> List.enum
    |> Enum.map Nondeterminism_monad.enum
    |> Enum.concat
  else
    Enum.empty ()
;;
