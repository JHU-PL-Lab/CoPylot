open Format
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
and pp_stmt indent fmt {uid=uid;exception_target=exc;multi=multi;body} =
  fprintf fmt "%a:" pp_label(uid,exc,multi);
  fprintf fmt "%s" indent;
  begin
    match body with
    | Assign (target,value) ->
      fprintf fmt "%a = %a"
        pp_id target
        pp_compound_expr value
    | FunctionDef (name,args,body) ->
      (* Python flavor FunctionDef: *)
      (* fprintf fmt "@[<4>%a:%a(%a):@\n%a@]" *)
      fprintf fmt "def %a(%a) {@\n%a@\n}"
        pp_id name
        (pp_list pp_id) args
        (pp_lines (pp_stmt (indent ^ "  "))) body
    | Return (value) ->
      fprintf fmt "return %a"
        (pp_option pp_id) value
    | Print (_,values,_) ->
      fprintf fmt "print %a" (* TODO: print dest if relevant *)
        (pp_list pp_id) values
    (* (pp_option pp_simple_expr) dest *)
    | Raise (value) ->
      fprintf fmt "raise %a"
        pp_id value
    | Catch (name) ->
      fprintf fmt "catch %a"
        pp_id name
    | Pass ->
      fprintf fmt "pass"
    | Goto (dest) ->
      fprintf fmt "goto %a"
        pp_print_int dest
    | GotoIfNot (test,dest) ->
      fprintf fmt "goto %a if not %a"
        pp_print_int dest
        pp_id test
    | NameStmt (name) ->
      pp_id fmt name
  end;
  fprintf fmt "%s" ";"

and pp_compound_expr fmt {uid=_;exception_target=_;multi=_;body} =
  match body with
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
  | Literal (l) -> pp_literal fmt l
  | Name (id)   -> pp_id fmt id


and pp_id fmt id =
  fprintf fmt "%s" id

and pp_literal fmt = function
  | Num (n)      -> pp_num fmt n
  | Str (s)      -> pp_str fmt s
  | Bool (b)     -> pp_print_bool fmt b
  | Builtin (bi) -> pp_builtin fmt bi

and pp_num fmt = function
  | Int sgn   -> fprintf fmt "Int%a" pp_sign sgn
  | Float sgn -> fprintf fmt "Float%a" pp_sign sgn

and pp_str fmt = function
  | StringAbstract -> fprintf fmt "StringAbstract"
  | StringLiteral s -> fprintf fmt "\"%s\"" (String.escaped s)

and pp_sign fmt= function
  | Pos  -> fprintf fmt "+"
  | Neg  -> fprintf fmt "-"
  | Zero -> fprintf fmt "0"

and pp_builtin fmt = function
  | Builtin_slice -> fprintf fmt "slice"
  | Builtin_bool  -> fprintf fmt "bool"
  | Builtin_type  -> fprintf fmt "type"

and pp_multi fmt m =
  if m then fprintf fmt "T" else fprintf fmt "F"

and pp_exc fmt = function
  | None    -> fprintf fmt "%s" "    "
  | Some(n) -> fprintf fmt "%4d" n

and pp_label fmt (uid,exc,multi) =
  fprintf fmt "@%4d:%a:%a"
    uid
    pp_exc exc
    pp_multi multi
;;
