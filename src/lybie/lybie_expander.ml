open Batteries;;
module Lamia = Lamia_ast;;
module Macros = Lybie_ast;;
(* TODO: Probably gonna want lamia_conversion_monad *)

let map_and_concat f lst = List.concat @@ List.map f lst;;

let rec expand_macros_block (m : 'a Macros.block) : 'a Lamia.block =
  let Macros.Block(stmts) = m in
  Lamia.Block(map_and_concat expand_macros_stmt stmts)

and expand_macros_stmt
    (stmt : 'a Macros.statement)
  : 'a Lamia.statement list =
  match stmt with
  | Macros.Statement (annot, d) ->
    [Lamia.Statement(annot, expand_macros_directive d)]
  | Macros.Stmt_macro(annot, macro) ->
    expand_macro annot macro

and expand_macros_directive
    (d : 'a Macros.directive)
  : 'a Lamia.directive =
  match d with
  | Macros.Let_expression (x,e) ->
    Lamia.Let_expression(x, expand_macros_valexp e)
  | Macros.Let_alloc y ->
    Lamia.Let_alloc y
  | Macros.Let_alias_value (x1,x2) ->
    Lamia.Let_alias_value (x1,x2)
  | Macros.Let_alias_memory (y1,y2) ->
    Lamia.Let_alias_memory (y1,y2)
  | Macros.Let_binding_update (x1,x2,x3,y) ->
    Lamia.Let_binding_update (x1,x2,x3,y)
  | Macros.Let_binding_access (y,x1,x2) ->
    Lamia.Let_binding_access (y,x1,x2)
  | Macros.Let_list_access (y,x1,x2) ->
    Lamia.Let_list_access (y,x1,x2)
  | Macros.Let_list_slice (y1,y2,y3,y4) ->
    Lamia.Let_list_slice (y1,y2,y3,y4)
  | Macros.Let_call_function (y,x,xs) ->
    Lamia.Let_call_function (y,x,xs)
  | Macros.Store (y,x) ->
    Lamia.Store (y,x)
  | Macros.Let_get (x,y) ->
    Lamia.Let_get (x,y)
  | Macros.Let_is (x,y1,y2) ->
    Lamia.Let_is (x,y1,y2)
  | Macros.Let_unop (x1,op,x2) ->
    Lamia.Let_unop (x1,op,x2)
  | Macros.Let_binop (x1,x2,op,x3) ->
    Lamia.Let_binop (x1,x2,op,x3)
  | Macros.Return y ->
    Lamia.Return y
  | Macros.If_result_value x ->
    Lamia.If_result_value x
  | Macros.If_result_memory y ->
    Lamia.If_result_memory y
  | Macros.Raise y ->
    Lamia.Raise y
  | Macros.Try_except (body,y,handler) ->
    Lamia.Try_except (expand_macros_block body, y, expand_macros_block handler)
  | Macros.Let_conditional_value (x1,x2,body,orelse) ->
    Lamia.Let_conditional_value (x1, x2, expand_macros_block body, expand_macros_block orelse)
  | Macros.Let_conditional_memory (y1,y2,body,orelse) ->
    Lamia.Let_conditional_memory (y1, y2, expand_macros_block body, expand_macros_block orelse)
  | Macros.While (test, body) ->
    Lamia.While (test, expand_macros_block body)

and expand_macros_valexp
    (v : 'a Macros.value_expression)
  : 'a Lamia.value_expression =
  match v with
  | Macros.Integer_literal n ->
    Lamia.Integer_literal n
  | Macros.String_literal s ->
    Lamia.String_literal s
  | Macros.Boolean_literal b ->
    Lamia.Boolean_literal b
  | Macros.List_expression lst ->
    Lamia.List_expression lst
  | Macros.Function_expression (args,body) ->
    Lamia.Function_expression(args, expand_macros_block body)
  | Macros.None_literal ->
    Lamia.None_literal
  | Macros.Empty_binding ->
    Lamia.Empty_binding

(* The real stuff starts here *)
and expand_macro
    (annot : 'a)
    (m : Macros.stmt_macro)
  : 'a Lamia.statement list =
  (* let open Lamia_conversion_monad in *)
  ignore annot;
  match m with
  | Macros.Unpack_python_args(args) ->
    ignore args; failwith "NYI"






























;;
