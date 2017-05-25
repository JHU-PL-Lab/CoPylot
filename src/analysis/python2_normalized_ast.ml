type uid = int
[@@deriving eq, ord, show]
;;

type identifier = string
[@@deriving eq, ord, show]

and modl =
    | Module of stmt list (* body *) * uid
[@@deriving eq, ord, show]

and stmt =
    | Assign of simple_expr (* target *) * compound_expr (* value *) * uid
  | FunctionDef of identifier (* name *) * arguments (* args *) * stmt list (* body *) * simple_expr list (* decorator_list *) * uid
  | Return of simple_expr option (* value *) * uid

  | Print of simple_expr option (* dest *) * simple_expr list (* values *) * bool (* nl *) * uid
  | If of simple_expr (* test *) * stmt list (* body *) * stmt list (* orelse *) * uid
  | Pass of uid (*
    TODO: Figure this out *)
  | Goto of uid
  | SimpleExprStmt of simple_expr (* value *) * uid
[@@deriving eq, ord, show]

and compound_expr =
    | BoolOp of simple_expr (* left *) * boolop (* op *) * simple_expr (* right *) * uid
  | BinOp of simple_expr (* left *) * operator (* op *) * simple_expr (* right *) * uid
  | UnaryOp of unaryop (* op *) * simple_expr (* operand *) * uid

  | IfExp of simple_expr (* test *) * simple_expr (* body *) * simple_expr (* orelse *) * uid
  | Compare of simple_expr (* left *) * cmpop (* ops *) * simple_expr (* comparators *) * uid
  | Call of simple_expr (* func *) * simple_expr list (* args *) *  uid
  | Subscript of simple_expr (* value *) * slice (* slice *)  * uid
  | List of simple_expr list (* elts *)  * uid
  | Tuple of simple_expr list (* elts *)  * uid
  | SimpleExpr of simple_expr (* value*) * uid
[@@deriving eq, ord, show]

and simple_expr =
    | Num of number (* n *) * uid
  | Str of uid
  | Bool of bool * uid
  | Name of identifier (* id *) * uid
[@@deriving eq, ord, show]

and slice = (* TODO: Remove? Probably *)
    | Slice of simple_expr option (* lower *) * simple_expr option (* upper *) * simple_expr option (* step *)
  | Index of simple_expr (* value *)
[@@deriving eq, ord, show]

and boolop = And | Or
[@@deriving eq, ord, show]

and operator = Add | Sub | Mult | Div | Mod | Pow
[@@deriving eq, ord, show]

and unaryop = Not | UAdd | USub
[@@deriving eq, ord, show]

and cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | In | NotIn
[@@deriving eq, ord, show]

and arguments = simple_expr list (* args *) * identifier option (* varargs *) * identifier option (* kwargs *) * simple_expr list (* defaults *)
[@@deriving eq, ord, show]

and keyword = identifier (* arg *) * simple_expr (* value *)
[@@deriving eq, ord, show]

and sign = Pos | Neg | Zero
[@@deriving eq, ord, show]

and number =
    | Int of sign
  | Float of sign
[@@deriving eq, ord, show]

let uid_of_simple_expr = function
  | Num (_, u)
  | Str (u)
  | Bool (_, u)
  | Name (_, u)
    -> u

and uid_of_compound_expr = function
  | BoolOp (_,_,_,u)
  | BinOp (_,_,_,u)
  | UnaryOp (_,_,u)
  | IfExp (_,_,_,u)
  | Compare (_,_,_,u)
  | Call (_,_,u)
  | Subscript (_,_,u)
  | List (_,u)
  | Tuple (_,u)
  | SimpleExpr (_,u)
    -> u
