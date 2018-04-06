open Python2_ast_types;;

type annotated_modl = modl annotation
[@@deriving eq, ord, show, to_yojson]

and annotated_stmt = stmt annotation
[@@deriving eq, ord, show, to_yojson]

and annotated_expr = expr annotation
[@@deriving eq, ord, show, to_yojson]

and modl =
    | Module of annotated_stmt list (* body *)
[@@deriving eq, ord, show, to_yojson]

and stmt =
    | Assign of identifier (* target *) * annotated_expr (* value *)
  | Return of identifier (* value *)
  | While of identifier (* test *) * annotated_stmt list (* body *)
  | If of identifier (* test *) * annotated_stmt list (* body *) * annotated_stmt list (* orelse *)
  | Raise of identifier (* value *)
  | TryExcept of annotated_stmt list (* body *) * identifier (* exn name *) * annotated_stmt list (* handler *)
  | Pass
  | Break
  | Continue
[@@deriving eq, ord, show, to_yojson]

and expr =
    | Binop of identifier (* left *) * binop (* op *) * identifier (* right *)
  | UnaryOp of unaryop (* op *) * identifier (* value *)
  | Call of identifier (* func *) * identifier list (* args *)
  | Attribute of identifier (* object *) * identifier (* attr *)
  | List of identifier list (* elts *)
  | Tuple of identifier list (* elts *)
  | Num of number
  | Str of string
  | Bool of bool
  | Builtin of builtin
  | FunctionVal of identifier list (* args *) * annotated_stmt list (* body *)
  | Name of identifier (* id *)
[@@deriving eq, ord, show, to_yojson]

and binop = Is
[@@deriving eq, ord, show, to_yojson]

and unaryop = Not
[@@deriving eq, ord, show, to_yojson]
