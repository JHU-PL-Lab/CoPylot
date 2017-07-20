open Batteries;;
open Jhupllib;;
open Nondeterminism;;

open Analysis_grammar;;
open Analysis_types;;
open Analysis_lookup_dph.Dph;;
(* open State;; *)
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
    [
      begin
        let%orzero Stmt (Statement(_, Let_expression (x,e))) = src in
        match e with
        | Integer_literal sgn ->
          return [Pop (Lookup_value_variable x); Push (Lookup_value (Integer sgn))]
        | String_literal str ->
          return [Pop (Lookup_value_variable x); Push (Lookup_value (String str))]
        | Boolean_literal b ->
          return [Pop (Lookup_value_variable x); Push (Lookup_value (Boolean b))]
        | List_value _ -> raise @@ Utils.Not_yet_implemented "per_cfg_edge_function: list_value"
        | Function_expression (args, block) ->
          return [Pop (Lookup_value_variable x); Push (Lookup_value (Function (args, block)))]
        | None_literal ->
          return [Pop (Lookup_value_variable x); Push (Lookup_value (None_value))]
        | Empty_binding ->
          return [Pop (Lookup_value_variable x); Push (Lookup_value (Empty_binding_value))]
      end;
      begin
        let%orzero Stmt (Statement(uid, Let_alloc y)) = src in
        return [Pop (Lookup_memory_variable y); Push (Lookup_memory (Memloc (Statement(uid, Let_alloc y))))]
      end;
      begin
        let%orzero Stmt (Statement(_, Let_alias_value (x1, x2))) = src in
        return [Pop (Lookup_value_variable x1); Push (Lookup_value_variable x2)]
      end;
      begin
        let%orzero Stmt (Statement(_, Let_alias_memory (y1, y2))) = src in
        return [Pop (Lookup_memory_variable y1); Push (Lookup_memory_variable y2)]
      end;
      begin
        let%orzero Stmt (Statement(_, Let_binding_update (b, b', x, y))) = src in
        return [Pop (Lookup_value_variable b); Push (Lookup_bind); Push (Lookup_jump dst); Push (Lookup_capture 3); Push(Lookup_memory_variable y); Push (Lookup_jump dst); Push (Lookup_capture 5); Push(Lookup_value_variable x); Push (Lookup_jump dst); Push (Lookup_capture 7); Push(Lookup_value_variable b')]
      end;
      begin
        let%orzero Stmt (Statement(_, Let_binding_access (y,b,x))) = src in
        return [Pop (Lookup_memory_variable y); Push (Lookup_project); Push (Lookup_jump dst); Push (Lookup_capture 3); Push(Lookup_value_variable x); Push (Lookup_jump dst); Push (Lookup_capture 5); Push(Lookup_value_variable b)]
      end;
      begin
        let%orzero Stmt (Statement(_, Let_list_access (y,lst,i))) = src in
        return [Pop (Lookup_memory_variable y); Push (Lookup_index); Push (Lookup_jump dst); Push (Lookup_capture 3); Push(Lookup_value_variable i); Push (Lookup_jump dst); Push (Lookup_capture 5); Push(Lookup_value_variable lst)]
      end;
      (* begin
        (* TODO: check stack *)
        let%orzero Stmt (Statement(_, Store (y,x))) = src in
        return [Push (Lookup_isalias); Push (Lookup_jump dst); Push (Lookup_capture 2); Push(Lookup_memory_variable y)]
      end; *)
      begin
        let%orzero Stmt (Statement(_, Let_get (x,y))) = src in
        return [Pop (Lookup_value_variable x); Push (Lookup_dereference); Push (Lookup_jump dst); Push (Lookup_capture 1); Push(Lookup_memory_variable y)]
      end;
      begin
        let%orzero Stmt (Statement(_, Let_is (x,y1,y2))) = src in
        return [Pop (Lookup_value_variable x); Push (Lookup_is); Push (Lookup_jump dst); Push (Lookup_capture 3); Push(Lookup_memory_variable y2); Push (Lookup_jump dst); Push (Lookup_capture 5); Push(Lookup_memory_variable y1)]
      end;
      begin
        let%orzero Stmt (Statement(_, Let_unop (x,_,x'))) = src in
        return [Pop (Lookup_value_variable x); Push (Lookup_unop); Push (Lookup_jump dst); Push (Lookup_capture 3); Push(Lookup_value_variable x')]
      end;
      begin
        let%orzero Stmt (Statement(_, Let_binop (x,x1,_,x2))) = src in
        return [Pop (Lookup_value_variable x); Push (Lookup_binop); Push (Lookup_jump dst); Push (Lookup_capture 3); Push(Lookup_value_variable x2); Push (Lookup_jump dst); Push (Lookup_capture 5); Push(Lookup_value_variable x1)]
      end;

      begin
        let%orzero Stmt (Statement(uid, While (y,block))) = src in
        let%orzero Advance _ = dst in
        return [Push (Lookup_jump (Stmt (Statement(uid, While (y,block)))))]
      end;
    ]
    |> List.enum
    |> Enum.map Nondeterminism_monad.enum
    |> Enum.concat
  else
    Enum.empty ()
;;
