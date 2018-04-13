open Batteries;;

let rec convert_value_expression =
  function
  | Lamia_ast.Integer_literal n -> Lamia_evaluation_ast.Integer_literal n
  | Lamia_ast.String_literal s -> Lamia_evaluation_ast.String_literal s
  | Lamia_ast.Boolean_literal b -> Lamia_evaluation_ast.Boolean_literal b
  | Lamia_ast.List_expression lst -> Lamia_evaluation_ast.List_value lst
  | Lamia_ast.Function_expression(xs,b) ->
    Lamia_evaluation_ast.Function_expression(xs, convert_block b)
  | Lamia_ast.None_literal -> Lamia_evaluation_ast.None_literal
  | Lamia_ast.NotImplemented_literal -> Lamia_evaluation_ast.NotImplemented_literal
  | Lamia_ast.Empty_binding -> Lamia_evaluation_ast.Empty_binding

and convert_directive =
  function
  | Lamia_ast.Let_expression(x,e) ->
    Lamia_evaluation_ast.Let_expression(x,convert_value_expression e)
  | Lamia_ast.Let_alloc y ->
    Lamia_evaluation_ast.Let_alloc y
  | Lamia_ast.Let_alias_value(x1,x2) ->
    Lamia_evaluation_ast.Let_alias_value(x1,x2)
  | Lamia_ast.Let_alias_memory(y1,y2) ->
    Lamia_evaluation_ast.Let_alias_memory(y1,y2)
  | Lamia_ast.Let_binding_update(x1,x2,x3,y) ->
    Lamia_evaluation_ast.Let_binding_update(x1,x2,x3,y)
  | Lamia_ast.Let_binding_access(y,x1,x2) ->
    Lamia_evaluation_ast.Let_binding_access(y,x1,x2)
  | Lamia_ast.Let_list_access(y,x1,x2) ->
    Lamia_evaluation_ast.Let_list_access(y,x1,x2)
  | Lamia_ast.Let_list_slice(x1,x2,x3,x4) ->
    Lamia_evaluation_ast.Let_list_slice(x1,x2,x3,x4)
  | Lamia_ast.Let_call_function(y,x,xs) ->
    Lamia_evaluation_ast.Let_call_function(y,x,xs)
  | Lamia_ast.Store(y,x) ->
    Lamia_evaluation_ast.Store(y,x)
  | Lamia_ast.Let_get(x,y) ->
    Lamia_evaluation_ast.Let_get(x,y)
  | Lamia_ast.Let_is(x,y1,y2) ->
    Lamia_evaluation_ast.Let_is(x,y1,y2)
  | Lamia_ast.Let_unop(x1,unop,x2) ->
    Lamia_evaluation_ast.Let_unop(x1,unop,x2)
  | Lamia_ast.Let_binop(x1,x2,binop,x3) ->
    Lamia_evaluation_ast.Let_binop(x1,x2,binop,x3)
  | Lamia_ast.Return(y) ->
    Lamia_evaluation_ast.Return(y)
  | Lamia_ast.If_result_value(x) ->
    Lamia_evaluation_ast.If_result_value(x)
  | Lamia_ast.If_result_memory(y) ->
    Lamia_evaluation_ast.If_result_memory(y)
  | Lamia_ast.Raise(y) ->
    Lamia_evaluation_ast.Raise(y)
  | Lamia_ast.Try_except(b1,y,b2,_) ->
    Lamia_evaluation_ast.Try_except(convert_block b1,y,convert_block b2)
  | Lamia_ast.Let_conditional_value(x1,x2,b1,b2) ->
    Lamia_evaluation_ast.Let_conditional_value(
      x1,x2,convert_block b1,convert_block b2)
  | Lamia_ast.Let_conditional_memory(y,x,b1,b2) ->
    Lamia_evaluation_ast.Let_conditional_memory(
      y,x,convert_block b1,convert_block b2)
  | Lamia_ast.While(y,b,_) ->
    Lamia_evaluation_ast.While(y,convert_block b)

and convert_statement =
  function
  | Lamia_ast.Statement(_,d) ->
    Lamia_evaluation_ast.Statement(convert_directive d)

and convert_block =
  function
  | Lamia_ast.Block(ss) ->
    Lamia_evaluation_ast.Block(List.map convert_statement ss)
;;
