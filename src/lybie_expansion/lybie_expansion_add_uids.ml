open Batteries;;
open Python2_ast_types;;
open Lamia_ast;;
open Uid_ctx;;

let rec add_uids_block (ctx : uid_context) (b : annot block) =
  let Block(stmts) = b in
  Block(List.map (add_uids_stmt ctx) stmts)

and add_uids_stmt ctx s =
  let Statement(a, d) = s in
  let new_uid = get_next_uid ctx a in
  let new_d = add_uids_directive ctx d in
  Statement(new_uid, new_d)

and add_uids_directive ctx d =
  match d with
  | Let_expression(arg1, e) ->
    Let_expression(arg1, add_uids_expression ctx e)
  | Let_alloc(arg1) ->
    Let_alloc(arg1)
  | Let_alias_value(arg1, arg2) ->
    Let_alias_value(arg1, arg2)
  | Let_alias_memory(arg1, arg2) ->
    Let_alias_memory(arg1, arg2)
  | Let_binding_update(arg1, arg2, arg3, arg4) ->
    Let_binding_update(arg1, arg2, arg3, arg4)
  | Let_binding_access(arg1, arg2, arg3) ->
    Let_binding_access(arg1, arg2, arg3)
  | Let_list_access(arg1, arg2, arg3) ->
    Let_list_access(arg1, arg2, arg3)
  | Let_list_slice(arg1, arg2, arg3, arg4) ->
    Let_list_slice(arg1, arg2, arg3, arg4)
  | Let_call_function(arg1, arg2, arg3) ->
    Let_call_function(arg1, arg2, arg3)
  | Store(arg1, arg2) ->
    Store(arg1, arg2)
  | Let_get(arg1, arg2) ->
    Let_get(arg1, arg2)
  | Let_is(arg1, arg2, arg3) ->
    Let_is(arg1, arg2, arg3)
  | Let_unop(arg1, arg2, arg3) ->
    Let_unop(arg1, arg2, arg3)
  | Let_binop(arg1, arg2, arg3, arg4) ->
    Let_binop(arg1, arg2, arg3, arg4)
  | Return(arg1) ->
    Return(arg1)
  | If_result_value(arg1) ->
    If_result_value(arg1)
  | If_result_memory(arg1) ->
    If_result_memory(arg1)
  | Raise(arg1) ->
    Raise(arg1)
  | Try_except(body, arg1, handler) ->
    Try_except(add_uids_block ctx body, arg1, add_uids_block ctx handler)
  | Let_conditional_value(arg1, arg2, body, orelse) ->
    Let_conditional_value(arg1, arg2, add_uids_block ctx body, add_uids_block ctx orelse)
  | Let_conditional_memory(arg1, arg2, body, orelse) ->
    Let_conditional_memory(arg1, arg2, add_uids_block ctx body, add_uids_block ctx orelse)
  | While(arg, body) ->
    While(arg, add_uids_block ctx body)

and add_uids_expression ctx e =
  match e with
  | Integer_literal(n) ->
    Integer_literal n
  | String_literal(s) ->
    String_literal s
  | Boolean_literal(b) ->
    Boolean_literal b
  | List_expression(lst) ->
    List_expression lst
  | Function_expression(args, body) ->
    Function_expression(args, add_uids_block ctx body)
  | None_literal ->
    None_literal
  | NotImplemented_literal ->
    NotImplemented_literal
  | Empty_binding ->
    Empty_binding
