open Python2_ast_types

type 'a modl =
    | Module of 'a stmt list (* body *) * 'a
[@@deriving eq, ord, show, to_yojson]

and 'a stmt =
    | Assign of identifier (* target *) * 'a expr (* value *) * 'a
  | Return of identifier (* value *) * 'a
  | While of identifier (* test *) * 'a stmt list (* body *) * 'a
  | If of identifier (* test *) * 'a stmt list (* body *) * 'a stmt list (* orelse *) * 'a
  | Raise of identifier (* value *) * 'a
  | TryExcept of 'a stmt list (* body *) * identifier (* exn name *) * 'a stmt list (* handler *) * 'a
  | Pass of 'a
  | Break of 'a
  | Continue of 'a
[@@deriving eq, ord, show, to_yojson]

and 'a expr =
    | Binop of identifier (* left *) * binop (* op *) * identifier (* right *) * 'a
  | UnaryOp of unaryop (* op *) * identifier (* value *) * 'a
  | Call of identifier (* func *) * identifier list (* args *) * 'a
  | Attribute of identifier (* object *) * identifier (* attr *) * 'a
  | List of identifier list (* elts *) * 'a
  | Tuple of identifier list (* elts *) * 'a
  | Num of number * 'a
  | Str of string * 'a
  | Bool of bool * 'a
  | Builtin of builtin * 'a
  | FunctionVal of identifier list (* args *) * 'a stmt list (* body *) * 'a
  | Name of identifier (* id *) * 'a
[@@deriving eq, ord, show, to_yojson]

and binop = Is
[@@deriving eq, ord, show, to_yojson]

and unaryop = Not
[@@deriving eq, ord, show, to_yojson]
