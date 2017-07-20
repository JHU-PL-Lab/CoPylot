module Lamia = Lamia_ast;;
module Abstract = Analysis_types;;
open Counter_hashtbl;;

let rec lift_block tbl b =
  let Lamia.Block(stmts) = b in
  Abstract.Block(List.map (lift_stmt tbl) stmts)

and lift_stmt tbl s =
  let Lamia.Statement(uid, directive) = s in
  let lifted_stmt = Abstract.Statement(uid, lift_directive tbl directive) in
  Counter_hashtbl.add tbl uid lifted_stmt;
  lifted_stmt

and lift_directive tbl d =
  match d with
  | Lamia.Let_expression(x, e) ->
    Abstract.Let_expression(lift_value_var x, lift_value_expression tbl e)
  | Lamia.Let_alloc(y) ->
    Abstract.Let_alloc(lift_memory_var y)
  | Lamia.Let_alias_value (x1, x2) ->
    Abstract.Let_alias_value (lift_value_var x1, lift_value_var x2)
  | Lamia.Let_alias_memory (y1, y2) ->
    Abstract.Let_alias_memory (lift_memory_var y1, lift_memory_var y2)
  | Lamia.Let_binding_update(x1, x2, x3, y1) ->
    Abstract.Let_binding_update(lift_value_var x1, lift_value_var x2, lift_value_var x3, lift_memory_var y1)
  | Lamia.Let_binding_access(y1, x1, x2) ->
    Abstract.Let_binding_access(lift_memory_var y1, lift_value_var x1, lift_value_var x2)
  | Lamia.Let_list_access(y1, x1, x2) ->
    Abstract.Let_list_access(lift_memory_var y1, lift_value_var x1, lift_value_var x2)
  | Lamia.Let_list_slice(x1, x2, x3, x4) ->
    Abstract.Let_list_slice(lift_value_var x1, lift_value_var x2, lift_value_var x3, lift_value_var x4)
  | Lamia.Let_call_function(y1, x1, xs) ->
    Abstract.Let_call_function(lift_memory_var y1, lift_value_var x1, List.map lift_value_var xs)
  | Lamia.Store(y1, x1) ->
    Abstract.Store(lift_memory_var y1, lift_value_var x1)
  | Lamia.Let_get(x1, y1) ->
    Abstract.Let_get(lift_value_var x1, lift_memory_var y1)
  | Lamia.Let_is(x1, y1, y2) ->
    Abstract.Let_is(lift_value_var x1, lift_memory_var y1, lift_memory_var y2)
  | Lamia.Let_unop(x1, op, x2) ->
    Abstract.Let_unop(lift_value_var x1, lift_unop op, lift_value_var x2)
  | Lamia.Let_binop(x1, x2, op, x3) ->
    Abstract.Let_binop(lift_value_var x1, lift_value_var x2, lift_binop op, lift_value_var x3)
  | Lamia.Return(y) ->
    Abstract.Return(lift_memory_var y)
  | Lamia.If_result_value(x) ->
    Abstract.If_result_value(lift_value_var x)
  | Lamia.If_result_memory(y) ->
    Abstract.If_result_memory(lift_memory_var y)
  | Lamia.Raise(y) ->
    Abstract.Raise(lift_memory_var y)
  | Lamia.Try_except(body, y, handler) ->
    Abstract.Try_except(lift_block tbl body, lift_memory_var y, lift_block tbl handler)
  | Lamia.Let_conditional_value(x1, x2, body, orelse) ->
    Abstract.Let_conditional_value(lift_value_var x1, lift_value_var x2, lift_block tbl body, lift_block tbl orelse)
  | Lamia.Let_conditional_memory(y, x, body, orelse) ->
    Abstract.Let_conditional_memory(lift_memory_var y, lift_value_var x, lift_block tbl body, lift_block tbl orelse)
  | Lamia.While(y, body) ->
    Abstract.While(lift_memory_var y, lift_block tbl body)

and lift_value_expression tbl e =
  match e with
  | Lamia.Integer_literal n ->
    let sign =
      if n > 0 then Abstract.Pos else
      if n < 0 then Abstract.Neg else
        Abstract.Zero
    in
    Abstract.Integer_literal sign
  | Lamia.String_literal s -> Abstract.String_literal(Abstract.String_exact s)
  | Lamia.Boolean_literal b -> Abstract.Boolean_literal b
  | Lamia.List_value lst -> Abstract.List_value(List.map lift_memory_var lst)
  | Lamia.Function_expression (args, body) ->
    Abstract.Function_expression (List.map lift_value_var args, lift_block tbl body)
  | Lamia.None_literal -> Abstract.None_literal
  | Lamia.Empty_binding -> Abstract.Empty_binding

and lift_unop op =
  match op with
  | Lamia.Unop_not -> Abstract.Unop_not
  | Lamia.Unop_is_function -> Abstract.Unop_is_function
  | Lamia.Unop_is_int -> Abstract.Unop_is_int

and lift_binop op =
  match op with
  | Lamia.Binop_intplus -> Abstract.Binop_intplus
  | Lamia.Binop_intminus -> Abstract.Binop_intminus
  | Lamia.Binop_haskey -> Abstract.Binop_haskey
  | Lamia.Binop_listconcat -> Abstract.Binop_listconcat
  | Lamia.Binop_equals -> Abstract.Binop_equals

and lift_value_var x =
  let Lamia.Value_variable(id) = x in
  Abstract.Value_variable(id)

and lift_memory_var x =
  let Lamia.Memory_variable(id) = x in
  Abstract.Memory_variable(id)
;;

let lift_block_top (b : Lamia.uid Lamia.block) =
  let empty_tbl = Counter_hashtbl.create 10 in
  let lifted_block = lift_block empty_tbl b in
  lifted_block, empty_tbl
;;
