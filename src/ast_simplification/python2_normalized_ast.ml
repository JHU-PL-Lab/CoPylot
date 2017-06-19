open Python2_ast_types

type annotated_stmt = stmt annotation
[@@deriving eq, ord, show, to_yojson]

and annotated_expr = expr annotation
[@@deriving eq, ord, show, to_yojson]

and modl =
    | Module of annotated_stmt list (* body *) * uid
[@@deriving eq, ord, show, to_yojson]

and stmt =
    | Assign of identifier (* target *) * annotated_expr (* value *)
  | Return of identifier (* value *)
  | Print of identifier option (* dest *) * identifier list (* values *) * bool (* nl *)
  | Raise of identifier (* value *)
  | Catch of identifier (* name *)
  | Pass
  | Goto of uid (* dest *)
  | GotoIfNot of identifier (* test *) * uid (* dest *) (* Jump iff test is falsey *)
  | NameStmt of identifier
[@@deriving eq, ord, show, to_yojson]

and expr =
    | Call of identifier (* func *) * identifier list (* args *)
  | Attribute of identifier (* object *) * identifier (* attr *)
  | List of identifier list (* elts *)
  | Tuple of identifier list (* elts *)
  | Literal of literal
  | Name of identifier (* id *)
[@@deriving eq, ord, show, to_yojson]

and literal =
    | Num of number
  | Str of str
  | Bool of bool
  | Builtin of builtin
  | FunctionVal of identifier list (* args *) * annotated_stmt list (* body *)
  | NoneVal
[@@deriving eq, ord, show, to_yojson]

and number =
    | Int of int
  | Float of float
[@@deriving eq, ord, show, to_yojson]

and str =
  | StringLiteral of string
[@@deriving eq, ord, show, to_yojson]

and builtin =
    | Builtin_slice
  | Builtin_bool
  | Builtin_type
[@@deriving eq, ord, show, to_yojson]
