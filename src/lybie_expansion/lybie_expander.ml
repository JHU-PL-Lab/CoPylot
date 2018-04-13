open Batteries;;
open Unique_name_ctx;;
open Lybie_macro_expansion;;
open Uid_ctx;;
open Lybie_expansion_add_uids;;
module Lamia = Lamia_ast;;
module Lybie = Lybie_ast;;

let map_and_concat f lst = List.concat @@ List.map f lst;;

let rec expand_macros_block (ctx : name_context) (m : 'a Lybie.block) : 'a Lamia.block =
  let Lybie.Block(stmts) = m in
  Lamia.Block(map_and_concat (expand_macros_stmt ctx) stmts)

and expand_macros_stmt
    (ctx : name_context)
    (stmt : 'a Lybie.statement)
  : 'a Lamia.statement list =
  match stmt with
  | Lybie.Statement (annot, Lybie.Macro m) ->
    map_and_concat (expand_macros_stmt ctx) (expand_macro ctx annot m)
  | Lybie.Statement (annot, d) ->
    [Lamia.Statement(annot, expand_macros_directive ctx d)]

and expand_macros_directive
    (ctx : name_context)
    (d : 'a Lybie.directive)
  : 'a Lamia.directive =
  match d with
  | Lybie.Let_expression (x,e) ->
    Lamia.Let_expression(x, expand_macros_valexp ctx e)
  | Lybie.Let_alloc y ->
    Lamia.Let_alloc y
  | Lybie.Let_alias_value (x1,x2) ->
    Lamia.Let_alias_value (x1,x2)
  | Lybie.Let_alias_memory (y1,y2) ->
    Lamia.Let_alias_memory (y1,y2)
  | Lybie.Let_binding_update (x1,x2,x3,y) ->
    Lamia.Let_binding_update (x1,x2,x3,y)
  | Lybie.Let_binding_access (y,x1,x2) ->
    Lamia.Let_binding_access (y,x1,x2)
  | Lybie.Let_list_access (y,x1,x2) ->
    Lamia.Let_list_access (y,x1,x2)
  | Lybie.Let_list_slice (y1,y2,y3,y4) ->
    Lamia.Let_list_slice (y1,y2,y3,y4)
  | Lybie.Let_call_function (y,x,xs) ->
    Lamia.Let_call_function (y,x,xs)
  | Lybie.Store (y,x) ->
    Lamia.Store (y,x)
  | Lybie.Let_get (x,y) ->
    Lamia.Let_get (x,y)
  | Lybie.Let_is (x,y1,y2) ->
    Lamia.Let_is (x,y1,y2)
  | Lybie.Let_unop (x1,op,x2) ->
    Lamia.Let_unop (x1,op,x2)
  | Lybie.Let_binop (x1,x2,op,x3) ->
    Lamia.Let_binop (x1,x2,op,x3)
  | Lybie.Return y ->
    Lamia.Return y
  | Lybie.If_result_value x ->
    Lamia.If_result_value x
  | Lybie.If_result_memory y ->
    Lamia.If_result_memory y
  | Lybie.Raise y ->
    Lamia.Raise y
  | Lybie.Try_except (body,y,handler,orelse) ->
    Lamia.Try_except (expand_macros_block ctx body, y, expand_macros_block ctx handler, expand_macros_block ctx orelse)
  | Lybie.Let_conditional_value (x1,x2,body,orelse) ->
    Lamia.Let_conditional_value (x1, x2, expand_macros_block ctx body, expand_macros_block ctx orelse)
  | Lybie.Let_conditional_memory (y1,y2,body,orelse) ->
    Lamia.Let_conditional_memory (y1, y2, expand_macros_block ctx body, expand_macros_block ctx orelse)
  | Lybie.While (test, body, orelse) ->
    Lamia.While (test, expand_macros_block ctx body, expand_macros_block ctx orelse)
  | Lybie.Macro _ ->
    failwith "Called expand_macros_directive with a Macro"

and expand_macros_valexp
    (ctx : name_context)
    (v : 'a Lybie.value_expression)
  : 'a Lamia.value_expression =
  match v with
  | Lybie.Integer_literal n ->
    Lamia.Integer_literal n
  | Lybie.String_literal s ->
    Lamia.String_literal s
  | Lybie.Boolean_literal b ->
    Lamia.Boolean_literal b
  | Lybie.List_expression lst ->
    Lamia.List_expression lst
  | Lybie.Function_expression (args,body) ->
    Lamia.Function_expression(args, expand_macros_block ctx body)
  | Lybie.None_literal ->
    Lamia.None_literal
  | Lybie.NotImplemented_literal ->
    Lamia.NotImplemented_literal
  | Lybie.Empty_binding ->
    Lamia.Empty_binding
;;

let annot_to_uid
    (b : Generic_conversion_monad.annot Lamia.block)
  : Lamia_ast_types.uid Lamia.block * uid_context =
  let uid_ctx = create_new_uid_ctx 0 in
  let uid_block = add_uids_block uid_ctx b in
  uid_block, uid_ctx
;;
