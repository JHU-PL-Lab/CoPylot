open Format
open Python2_ast_types
open Python2_normalized_ast

(* Default pretty printing *)

let rec pp_lines printer fmt = function
  | [] -> ()
  | [x] -> printer fmt x
  | x :: rest ->
    fprintf fmt "%a@\n%a" printer x (pp_lines printer) rest

and pp_option pp fmt = function
  | None -> ()
  | Some x -> pp fmt x

and pp_list pp fmt lst =
  let rec loop pp fmt = function
    | [] -> ()
    | [x] -> pp fmt x
    | x :: rest ->
      fprintf fmt "%a, %a" pp x (loop pp) rest
  in loop pp fmt lst
;;

let rec pp_modl fmt = function
  | Module (body, _) ->
    pp_lines (pp_stmt "  ") fmt body

(* The statements should have line number. *)
and pp_stmt indent fmt body =
  fprintf fmt "%s" indent; ignore body; (* TODO: Re-implement this! *)
  (* begin
    match body with
    | Assign (target,value,_) ->
      fprintf fmt "%a = %a"
        pp_id target
        (pp_compound_expr indent) value
    | Return (value,_) ->
      fprintf fmt "return %a"
        pp_id value
    (* | Print (_,values,_) ->
      fprintf fmt "print %a"
        (pp_list pp_id) values *)
    (* (pp_option pp_simple_expr) dest *)
    | Raise (value,_) ->
      fprintf fmt "raise %a"
        pp_id value
    | Pass _ ->
      fprintf fmt "pass"
  end; *)
  fprintf fmt "%s" ";"

and pp_compound_expr indent fmt body =
  ignore indent; ignore fmt; ignore body; (* TODO: Re-implement this! *)
  (* match body with
  | Binop (left,op,right) ->
    fprintf fmt "%a %a %a"
      pp_id left
      pp_binop op
      pp_id right
  | Call (func,args) ->
    fprintf fmt "%a(%a)"
      pp_id func
      (pp_list pp_id) args
  | Attribute (obj,attr) ->
    fprintf fmt "%a.%a"
      pp_id obj
      pp_id attr
  | List (lst) ->
    fprintf fmt "[%a]"
      (pp_list pp_id) lst
  | Tuple (lst) ->
    fprintf fmt "(%a)"
      (pp_list pp_id) lst
  | Literal (l) -> (pp_literal indent) fmt l
  | Name (id)   -> pp_id fmt id *)

and pp_id fmt id =
  if id = "None" || id = "True" || id = "False"
  then
    failwith (id ^ "is not a valid identifier!")
  else
    fprintf fmt "%s" id

and pp_literal indent fmt = function
  | Num (n)      -> pp_num fmt n
  | Str (s)      -> pp_str fmt s
  | Bool (b)     -> pp_print_bool fmt b
  | Builtin (bi) -> pp_builtin fmt bi
  | FunctionVal (args, body) -> pp_functionval indent fmt args body

and pp_num fmt = function
  | Int n   -> fprintf fmt "%d" n
  | Float f -> fprintf fmt "%f" f

and pp_str fmt = function
  | s -> fprintf fmt "\"%s\"" (String.escaped s)

and pp_binop fmt = function
  | Is -> fprintf fmt "%s" "is"

and pp_builtin fmt = function
  | Builtin_slice -> fprintf fmt "*slice"
  | Builtin_bool  -> fprintf fmt "*bool"
  | Builtin_type  -> fprintf fmt "*type"
  | Builtin_method_wrapper_type -> fprintf fmt "*method_wrapper_type"
  | Builtin_AttributeError -> fprintf fmt "*AttributeError"
  | Builtin_ValueError -> fprintf fmt "*ValueError"
  | Builtin_TypeError -> fprintf fmt "*TypeError"

and pp_functionval indent fmt args body =
  fprintf fmt "def (%a) {\n%a\n}"
    (pp_list pp_id) args
    (pp_lines (pp_stmt (indent ^ "  "))) body
