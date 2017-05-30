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
  | FunctionDef of identifier (* name *) * arguments (* args *) * stmt list (* body *) * uid
  | Return of simple_expr option (* value *) * uid

  | Print of simple_expr option (* dest *) * simple_expr list (* values *) * bool (* nl *) * uid
  | If of simple_expr (* test *) * stmt list (* body *) * stmt list (* orelse *) * uid
  | Pass of uid
  | Raise of simple_expr option (* type *) * simple_expr option (* value *) * uid
  | TryExcept of stmt list (* body *) * excepthandler list (* handlers *) * uid
  | Goto of uid * uid
  | SimpleExprStmt of simple_expr (* value *) * uid
[@@deriving eq, ord, show]

and compound_expr =
    | BoolOp of simple_expr (* left *) * boolop (* op *) * simple_expr (* right *) * uid
  | BinOp of simple_expr (* left *) * operator (* op *) * simple_expr (* right *) * uid
  | UnaryOp of unaryop (* op *) * simple_expr (* operand *) * uid

  | IfExp of simple_expr (* test *) * simple_expr (* body *) * simple_expr (* orelse *) * uid
  | Compare of simple_expr (* left *) * cmpop (* ops *) * simple_expr (* comparators *) * uid
  | Call of simple_expr (* func *) * simple_expr list (* args *) *  uid
  | Attribute of simple_expr (* object *) * identifier (* attr *) * uid
  | Subscript of simple_expr (* value *) * slice (* slice *)  * uid
  | List of simple_expr list (* elts *)  * uid
  | Tuple of simple_expr list (* elts *)  * uid
  | SimpleExpr of simple_expr (* value*) * uid
[@@deriving eq, ord, show]

and simple_expr =
    | Num of number (* n *) * uid
  | Str of str * uid
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

and excepthandler = ExceptHandler of simple_expr option (* type *) * simple_expr option (* name *) * stmt list (* body *) * uid
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

and str =
    | StringAbstract
  | StringLiteral of string
[@@deriving eq, ord, show]

let name_of_mod = function
  | Module _      -> "Module"

and name_of_stmt = function
  | FunctionDef _     -> "FunctionDef"
  | Return _          -> "Return"
  | Assign _          -> "Assign"
  | Print _           -> "Print"
  | If _              -> "If"
  | Raise _           -> "Raise"
  | TryExcept _       -> "TryExcept"
  | SimpleExprStmt _  -> "SimpleExprStmt"
  | Pass _            -> "Pass"
  | Goto _            -> "Goto"

and name_of_cmpound_expr = function
  | BoolOp _       -> "BoolOp"
  | BinOp _        -> "BinOp"
  | UnaryOp _      -> "UnaryOp"
  | IfExp _        -> "IfExp"
  | Compare _      -> "Compare"
  | Call _         -> "Call"
  | Attribute _    -> "Attribute"
  | Subscript _    -> "Subscript"
  | List _         -> "List"
  | Tuple _        -> "Tuple"
  | SimpleExpr _   -> "SimpleExpr"

and name_of_simple_expr = function
  | Num _          -> "Num"
  | Str _          -> "Str"
  | Bool _         -> "Bool"
  | Name _         -> "Name"

and name_of_slice = function
  | Slice _     -> "Slice"
  | Index _     -> "Index"

and name_of_boolop = function
  | And -> "And"
  | Or  -> "Or"

and name_of_operator = function
  | Add         -> "Add"
  | Sub         -> "Sub"
  | Mult        -> "Mult"
  | Div         -> "Div"
  | Mod         -> "Mod"
  | Pow         -> "Pow"

and name_of_unaryop = function
  | Not         -> "Not"
  | UAdd        -> "UAdd"
  | USub        -> "USub"

and name_of_cmpop = function
  | Eq          -> "Eq"
  | NotEq       -> "NotEq"
  | Lt          -> "Lt"
  | LtE         -> "LtE"
  | Gt          -> "Gt"
  | GtE         -> "GtE"
  | In          -> "In"
  | NotIn       -> "NotIn"

and name_of_number = function
  | Int _       -> "Int"
  | Float _     -> "Float"

and name_of_str = function
  | StringAbstract -> "StringAbstract"
  | StringLiteral _ -> "StringLiteral"

let uid_of_mod = function
  | Module (_, u)
    -> u

and uid_of_stmt = function
  | FunctionDef (_, _, _, u)
  | Return (_, u)
  | Assign (_, _, u)
  | Print (_, _, _, u)
  | If (_, _, _, u)
  | Raise (_, _, u)
  | TryExcept (_, _, u)
  | SimpleExprStmt (_, u)
  | Pass (u)
  | Goto (_, u)
    -> u

and uid_of_compound_expr = function
  | BoolOp (_, _, _, u)
  | BinOp (_, _, _, u)
  | UnaryOp (_, _, u)
  | IfExp (_, _, _, u)
  | Compare (_, _, _, u)
  | Call (_, _, u)
  | Attribute (_, _, u)
  | Subscript (_, _, u)
  | List (_, u)
  | Tuple (_, u)
  | SimpleExpr (_, u)
    -> u

and uid_of_simple_expr = function
  | Num (_, u)
  | Str (_, u)
  | Bool (_, u)
  | Name (_, u)
    -> u

let string_of_boolop = function
  | And -> "and"
  | Or  -> "or"

let string_of_operator = function
  | Add         -> "+"
  | Sub         -> "-"
  | Mult        -> "*"
  | Div         -> "/"
  | Mod         -> "%"
  | Pow         -> "**"

let string_of_unaryop = function
  | Not    -> "not"
  | UAdd   -> "+"
  | USub   -> "-"

let string_of_cmpop = function
  | Eq    -> "=="
  | NotEq -> "!="
  | Lt    -> "<"
  | LtE   -> "<="
  | Gt    -> ">"
  | GtE   -> ">="
  | In    -> "in"
  | NotIn -> "not in"

let string_of_sign = function
  | Pos -> "Pos"
  | Neg -> "Neg"
  | Zero -> "Zero"

let string_of_number = function
  | Int (sgn)
  | Float (sgn) ->
    string_of_sign sgn

let string_of_str = function
  | StringAbstract -> "StringAbstract"
  | StringLiteral (s) -> s
