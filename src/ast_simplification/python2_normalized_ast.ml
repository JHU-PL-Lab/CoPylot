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
  | Raise of simple_expr (* value *) * uid * uid option (* exception label *)
  | Catch of identifier (* name *) * uid * uid option (* exception label *)
  | Pass of uid * uid option (* exception label *)
  | Goto of uid (* dest *) * uid * uid option (* exception label *)
  | GotoIfNot of simple_expr (* test *) * uid (* dest *) * uid * uid option (* Jump iff test is falsey *)
  | SimpleExprStmt of simple_expr (* value *) * uid * uid option (* exception label *)
[@@deriving eq, ord, show, to_yojson]

and compound_expr =
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
    | Num of number * uid * uid option (* exception label *)
  | Str of str * uid * uid option (* exception label *)
  | Bool of bool * uid * uid option (* exception label *)
  | Builtin of builtin * uid * uid option (* exception label *)
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

and builtin =
    | Builtin_slice
  | Builtin_bool
  | Builtin_type
[@@deriving eq, ord, show, to_yojson]
