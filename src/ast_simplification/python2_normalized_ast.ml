type uid = int
[@@deriving eq, ord, show, to_yojson]
;;

type identifier = string
[@@deriving eq, ord, show, to_yojson]

and modl =
    | Module of stmt list (* body *) * uid
[@@deriving eq, ord, show, to_yojson]

and stmt =
    | Assign of identifier (* target *) * compound_expr (* value *) * uid * uid option (* exception label *)
  | FunctionDef of identifier (* name *) * identifier list (* args *) * stmt list (* body *) * uid * uid option (* exception label *)
  | Return of simple_expr option (* value *) * uid * uid option (* exception label *)
  | Print of simple_expr option (* dest *) * simple_expr list (* values *) * bool (* nl *) * uid * uid option (* exception label *)
  | If of simple_expr (* test *) * stmt list (* body *) * stmt list (* orelse *) * uid * uid option (* exception label *)
  | Raise of simple_expr (* value *) * uid * uid option (* exception label *)
  | Catch of identifier (* name *) * uid * uid option (* exception label *)
  | Pass of uid * uid option (* exception label *)
  | Goto of uid * uid * uid option (* exception label *)
  | SimpleExprStmt of simple_expr (* value *) * uid * uid option (* exception label *)
[@@deriving eq, ord, show, to_yojson]

and compound_expr =
    | BoolOp of simple_expr (* left *) * boolop (* op *) * simple_expr (* right *) * uid * uid option (* exception label *)
  | BinOp of simple_expr (* left *) * operator (* op *) * simple_expr (* right *) * uid * uid option (* exception label *)
  | UnaryOp of unaryop (* op *) * simple_expr (* operand *) * uid * uid option (* exception label *)
  | Compare of simple_expr (* left *) * cmpop (* ops *) * simple_expr (* comparators *) * uid * uid option (* exception label *)
  | Call of simple_expr (* func *) * simple_expr list (* args *) * uid * uid option (* exception label *)
  | Attribute of simple_expr (* object *) * identifier (* attr *) * uid * uid option (* exception label *)
  | List of simple_expr list (* elts *)  * uid * uid option (* exception label *)
  | Tuple of simple_expr list (* elts *)  * uid * uid option (* exception label *)
  | SimpleExpr of simple_expr (* value*) * uid * uid option (* exception label *)
[@@deriving eq, ord, show, to_yojson]

and simple_expr =
    | Literal of literal * uid * uid option
  | Name of identifier (* id *) * uid * uid option (* exception label *)
[@@deriving eq, ord, show, to_yojson]

and literal =
    | Num of number (* n *) * uid * uid option (* exception label *)
  | Str of str * uid * uid option (* exception label *)
  | Bool of bool * uid * uid option (* exception label *)
[@@deriving eq, ord, show, to_yojson]

and boolop = And | Or
[@@deriving eq, ord, show, to_yojson]

and operator = Add | Sub | Mult | Div | Mod | Pow
[@@deriving eq, ord, show, to_yojson]

and unaryop = Not | UAdd | USub
[@@deriving eq, ord, show, to_yojson]

and cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | In | NotIn
[@@deriving eq, ord, show, to_yojson]

and sign = Pos | Neg | Zero
[@@deriving eq, ord, show, to_yojson]

and number =
    | Int of sign
  | Float of sign
[@@deriving eq, ord, show, to_yojson]

and str =
    | StringAbstract
  | StringLiteral of string
[@@deriving eq, ord, show, to_yojson]
