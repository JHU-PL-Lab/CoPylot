open Python2_ast_types

type 'a modl =
    | Module of 'a stmt list (* body *) * 'a
[@@deriving eq, ord, show]

and 'a stmt =
    | Assign of identifier (* target *) * 'a expr (* value *) * 'a
  | Return of 'a expr option (* value *) * 'a
  | Print of 'a expr option (* dest *) * 'a expr list (* values *) * bool (* nl *) * 'a
  | While of 'a expr (* test *) * 'a stmt list (* body *) * 'a
  | If of 'a expr (* test *) * 'a stmt list (* body *) * 'a stmt list (* orelse *) * 'a
  (* Raise is very complicated, with different behaviors based on the
     number of arguments it recieves. For simplicity we require that
     it take exactly one argument, which is the value to be raised. *)
  | Raise of 'a expr (* value *) * 'a
  | TryExcept of 'a stmt list (* body *) * 'a excepthandler list (* handlers *) * 'a
  | Pass of 'a
  | Break of 'a
  | Continue of 'a
  | Expr of 'a expr (* value *) * 'a
[@@deriving eq, ord, show]

and 'a expr =
    | BoolOp of boolop (* op *) * 'a expr list (* values *) * 'a
  | IfExp of 'a expr (* test *) * 'a expr (* body *) * 'a expr (* orelse *) * 'a
  | Compare of 'a expr (* left *) * cmpop list (* ops *) * 'a expr list (* comparators *) * 'a
  | Call of 'a expr (* func *) * 'a expr list (* args *) * 'a
  | Attribute of 'a expr (* object *) * identifier (* attr *) * 'a
  | List of 'a expr list (* elts *)  * 'a
  | Tuple of 'a expr list (* elts *)  * 'a
  | Num of number (* n *) * 'a
  | Str of str * 'a
  | Bool of bool * 'a
  | Name of identifier (* id *) * 'a
  | Builtin of builtin * 'a
  | NoneExpr of 'a
  | FunctionVal of identifier list (* args *) * 'a stmt list (* body *) * 'a
[@@deriving eq, ord, show]

and boolop = And | Or
[@@deriving eq, ord, show]

and cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | In | NotIn
[@@deriving eq, ord, show]

and 'a excepthandler = ExceptHandler of 'a expr option (* type *) * identifier option (* name *) * 'a stmt list (* body *) * 'a
[@@deriving eq, ord, show]

and number =
    | Int of int
  | Float of float
[@@deriving eq, ord, show]

and str =
  | StringLiteral of string
[@@deriving eq, ord, show]

and builtin =
    | Builtin_bool
  | Builtin_slice
  | Builtin_type
[@@deriving eq, ord, show]

let extract_stmt_annot = function
  | Assign (_, _, a)
  | Return(_,a)
  | Print(_,_,_,a)
  | While(_,_,a)
  | If(_,_,_,a)
  | Raise(_,a)
  | TryExcept(_,_,a)
  | Pass(a)
  | Break(a)
  | Continue(a)
  | Expr(_,a)
    -> a

let extract_expr_annot = function
  | BoolOp (_,_,a)
  | IfExp(_,_,_,a)
  | Compare(_,_,_,a)
  | Call(_,_,a)
  | Attribute(_,_,a)
  | List(_,a)
  | Tuple(_,a)
  | Num (_,a)
  | Str(_,a)
  | Bool(_,a)
  | Name(_,a)
  | Builtin(_,a)
  | NoneExpr (a)
  | FunctionVal (_,_,a)
    -> a
