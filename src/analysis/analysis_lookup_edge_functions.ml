open Batteries;;
open Jhupllib;;
open Nondeterminism;;

open Analysis_grammar;;
open Analysis_types;;
open Analysis_lookup_dph.Dph;;
(* open Analysis_lexical_relations;; *)
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
    return ([], Dynamic_terminus(Udp_jump));
    return ([Pop_dynamic_targeted(Tdp_capture_1)], Static_terminus(state));
  ]
  |> List.enum
  |> Enum.map Nondeterminism_monad.enum
  |> Enum.concat
;;

let per_cfg_edge_function src dst state =
  if Program_state.equal state dst then
    let open Nondeterminism_monad in
    let o0 = Program_state(dst) in
    let o1 = Program_state(src) in
    [
      (* Let x = e *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Let_expression (x,e)))) = o1 in
        match e with
        | Integer_literal sgn ->
          return ([Pop (Lookup_value_variable x); Push (Lookup_value (Integer sgn))], Static_terminus(o1))
        | String_literal str ->
          return ([Pop (Lookup_value_variable x); Push (Lookup_value (String str))], Static_terminus(o1))
        | Boolean_literal b ->
          return ([Pop (Lookup_value_variable x); Push (Lookup_value (Boolean b))], Static_terminus(o1))
        | List_value _ -> raise @@ Utils.Not_yet_implemented "per_cfg_edge_function: list_value"
        | Function_expression (args, block) ->
          return ([Pop (Lookup_value_variable x); Push (Lookup_value (Function (args, block)))], Static_terminus(o1))
        | None_literal ->
          return ([Pop (Lookup_value_variable x); Push (Lookup_value (None_value))], Static_terminus(o1))
        | Empty_binding ->
          return ([Pop (Lookup_value_variable x); Push (Lookup_value (Empty_binding_value))], Static_terminus(o1))
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
      (* Let x = y1 == y2 *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Let_is (x,y1,y2)))) = o1 in
        return ([Pop (Lookup_value_variable x); Push (Lookup_is); Push (Lookup_jump dst); Push (Lookup_capture 3); Push(Lookup_memory_variable y2); Push (Lookup_jump dst); Push (Lookup_capture 5); Push(Lookup_memory_variable y1)], Static_terminus(o1))
      end;
      (* Let x = unop x' *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Let_unop (_,op,x')))) = o1 in
        return ([Pop_dynamic_targeted(Tdp_unop(op,x'))], Static_terminus(o0))
        (* return ([Pop (Lookup_value_variable x); Push (Lookup_unop); Push (Lookup_jump dst); Push (Lookup_capture 3); Push(Lookup_value_variable x')], dst) *)
      end;
      (* Let x = x1 binop x2 *)
      begin
        let%orzero Program_state (Stmt (Statement(_, Let_binop (x,x1,_,x2)))) = o1 in
        return ([Pop (Lookup_value_variable x); Push (Lookup_binop); Push (Lookup_jump dst); Push (Lookup_capture 3); Push(Lookup_value_variable x2); Push (Lookup_jump dst); Push (Lookup_capture 5); Push(Lookup_value_variable x1)], Static_terminus(o0))
      end;

      (* Skip While Loop *)
      begin
        let%orzero Program_state (Stmt (Statement(uid, While (y,block)))) = o1 in
        return ([Push (Lookup_jump (Stmt (Statement(uid, While (y,block)))))], Static_terminus(o1))
      end;
      (* In-while, reach block start *)
      begin
        let%orzero Program_state (Stmt (s)) = o1 in
        let%orzero Statement(uid, While (y,block)) = s in
        (* let%orzero Some (block_start) = Stmt_map.find s relations.down in *)
        (* TODO: use relations *)
        return ([Push (Lookup_jump (Stmt (Statement(uid, While (y,block)))))], Static_terminus(o1))
        (* TODO: null dst? *)
      end;
      (* Ifresult x *)
      begin
        let%orzero Program_state (Stmt (Statement(_, If_result_value _))) = o1 in
        let%orzero Advance (s) = dst in
        let%orzero Statement(_, Let_conditional_value (x,_,_,_)) = s in
        return ([], Dynamic_terminus(Udp_ifresult_x (x,o1,Stmt(s))))
      end;
      (* Ifresult y *)
      begin
        let%orzero Program_state (Stmt (Statement(_, If_result_memory _))) = o1 in
        let%orzero Advance (s) = dst in
        let%orzero Statement(_, Let_conditional_memory (y,_,_,_)) = s in
        return ([], Dynamic_terminus(Udp_ifresult_y (y,o1,Stmt(s))))
      end;
    ]
    |> List.enum
    |> Enum.map Nondeterminism_monad.enum
    |> Enum.concat
  else
    Enum.empty ()
;;
