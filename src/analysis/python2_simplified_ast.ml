type identifier = string
[@@deriving eq, ord, show]

and 'a modl =
    | Module of 'a stmt list (* body *) * 'a
[@@deriving eq, ord, show]

and 'a stmt =
    | Assign of identifier (* target *) * 'a expr (* value *) * 'a
  | FunctionDef of identifier (* name *) * identifier list (* args *) * 'a stmt list (* body *) * 'a
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
  | BinOp of 'a expr (* left *) * operator (* op *) * 'a expr (* right *) * 'a
  | UnaryOp of unaryop (* op *) * 'a expr (* operand *) * 'a
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
[@@deriving eq, ord, show]

and boolop = And | Or
[@@deriving eq, ord, show]

and operator = Add | Sub | Mult | Div | Mod | Pow
[@@deriving eq, ord, show]

and unaryop = Not | UAdd | USub
[@@deriving eq, ord, show]

and cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | In | NotIn
[@@deriving eq, ord, show]

and 'a excepthandler = ExceptHandler of 'a expr option (* type *) * identifier option (* name *) * 'a stmt list (* body *) * 'a
[@@deriving eq, ord, show]

and sign = Pos | Neg | Zero
[@@deriving eq, ord, show]

and number =
    | Int of sign
  | Float of sign
[@@deriving eq, ord, show]

and str =
    | StringAbstract
  | StringLiteral of string
[@@deriving eq, ord, show]
