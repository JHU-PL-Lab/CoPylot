open Format;;
open Lamia_ast;;
open Lamia_ast_types;;

(* Default pretty printing *)

let indent_per_level = 4;;
let increase_indent indent =
  indent ^ String.make indent_per_level ' '
;;

let rec pp_list pp fmt lst =
  let rec loop pp fmt = function
    | [] -> ()
    | [x] -> pp fmt x
    | x :: rest ->
      fprintf fmt "%a, %a" pp x (loop pp) rest
  in loop pp fmt lst
;;

let rec pp_block indent fmt = function
  | Block (body) ->
    List.iter ((pp_stmt indent) fmt) body

and pp_stmt indent fmt s =
  let Statement((u:uid), d) = s in
  fprintf fmt "%s@%4d: %a;\n" indent u
    (pp_directive indent) d

and pp_directive indent fmt d =
  match d with
  | Let_expression(x, e) ->
    fprintf fmt "let %a = %a"
      pp_value_var x
      (pp_expr indent) e
  | Let_alloc(y) ->
    fprintf fmt "let %a = alloc"
      pp_memory_var y
  | Let_alias_value (x1, x2) ->
    fprintf fmt "let %a = %a"
      pp_value_var x1
      pp_value_var x2
  | Let_alias_memory (y1, y2) ->
    fprintf fmt "let %a = %a"
      pp_memory_var y1
      pp_memory_var y2
  | Let_binding_update(x1, x2, x3, y1) ->
    fprintf fmt "let %a = %a{%a -> %a}"
      pp_value_var x1
      pp_value_var x2
      pp_value_var x3
      pp_memory_var y1
  | Let_binding_access(y1, x1, x2) ->
    fprintf fmt "let %a = %a{%a}"
      pp_memory_var y1
      pp_value_var x1
      pp_value_var x2
  | Let_list_access(y1, x1, x2) ->
    fprintf fmt "let %a = %a[%a]"
      pp_memory_var y1
      pp_value_var x1
      pp_value_var x2
  | Let_list_slice(x1, x2, x3, x4) ->
    fprintf fmt "let %a = %a[%a:%a]"
      pp_value_var x1
      pp_value_var x2
      pp_value_var x3
      pp_value_var x4
  | Let_call_function(y1, x1, xs) ->
    fprintf fmt "let %a = %a(%a)"
      pp_memory_var y1
      pp_value_var x1
      (pp_list pp_value_var) xs
  | Store(y1, x1) ->
    fprintf fmt "store %a %a"
      pp_memory_var y1
      pp_value_var x1
  | Let_get(x1, y1) ->
    fprintf fmt "let %a = get %a"
      pp_value_var x1
      pp_memory_var y1
  | Let_is(x1, y1, y2) ->
    fprintf fmt "let %a = %a is %a"
      pp_value_var x1
      pp_memory_var y1
      pp_memory_var y2
  | Let_unop(x1, op, x2) ->
    fprintf fmt "let %a = %a %a"
      pp_value_var x1
      pp_unop op
      pp_value_var x2
  | Let_binop(x1, x2, op, x3) ->
    fprintf fmt "let %a = %a %a %a"
      pp_value_var x1
      pp_value_var x2
      pp_binop op
      pp_value_var x3
  | Return(y) ->
    fprintf fmt "return %a"
      pp_memory_var y
  | If_result_value(x) ->
    fprintf fmt "ifresult %a"
      pp_value_var x
  | If_result_memory(y) ->
    fprintf fmt "ifresult %a"
      pp_memory_var y
  | Raise(y) ->
    fprintf fmt "raise %a"
      pp_memory_var y
  | Try_except(body, y, handler) ->
    fprintf fmt "try {\n%a\n%s} except %a {\n%a\n%s}"
      (pp_block (increase_indent indent)) body
      indent
      pp_memory_var y
      (pp_block (increase_indent indent)) handler
      indent

  | Let_conditional_value(x1, x2, body, orelse) ->
    fprintf fmt "let %a = if %a then {\n%a\n%s} else {\n%a\n%s}"
      pp_value_var x1
      pp_value_var x2
      (pp_block (increase_indent indent)) body
      indent
      (pp_block (increase_indent indent)) orelse
      indent
  | Let_conditional_memory(y, x, body, orelse) ->
    fprintf fmt "let %a = if %a then {\n%a\n%s} else {\n%a\n%s}"
      pp_memory_var y
      pp_value_var x
      (pp_block (increase_indent indent)) body
      indent
      (pp_block (increase_indent indent)) orelse
      indent
  | While(y, body) ->
    fprintf fmt "while %a {\n%a\n%s}"
      pp_memory_var y
      (pp_block (increase_indent indent)) body
      indent

and pp_expr indent fmt e =
  match e with
  | Integer_literal n -> fprintf fmt "%d" n
  | String_literal s -> fprintf fmt "\"%s\"" (String.escaped s)
  | Boolean_literal b -> fprintf fmt (if b then "True" else "False")
  | List_expression lst -> fprintf fmt "[%a]" (pp_list pp_memory_var) lst
  | Function_expression (args, body) ->
    fprintf fmt "def (%a) {\n%a\n%s}"
      (pp_list pp_value_var) args
      (pp_block (increase_indent indent)) body
      indent
  | None_literal -> fprintf fmt "None"
  | Empty_binding -> fprintf fmt "{}"

and pp_unop fmt op =
  match op with
  | Unop_not -> fprintf fmt "not"
  | Unop_is_function -> fprintf fmt "isfunc"
  | Unop_is_int -> fprintf fmt "isint"

and pp_binop fmt op =
  match op with
  | Binop_intplus -> fprintf fmt "int+"
  | Binop_intminus -> fprintf fmt "int-"
  | Binop_haskey -> fprintf fmt "haskey"
  | Binop_listconcat -> fprintf fmt "||"
  | Binop_equals -> fprintf fmt "=="

and pp_value_var fmt x =
  let Value_variable(id) = x in
  fprintf fmt "%s" id

and pp_memory_var fmt y =
  let Memory_variable(id) = y in
  fprintf fmt "%s" @@ id

let pp_block_top = pp_block "";;
