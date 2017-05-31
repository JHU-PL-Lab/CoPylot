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
  | Raise of 'a expr option (* type *) * 'a expr option (* value *) * 'a
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

and 'a excepthandler = ExceptHandler of identifier option (* type *) * identifier option (* name *) * 'a stmt list (* body *) * 'a
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
  | While _           -> "While"
  | If _              -> "If"
  | Raise _           -> "Raise"
  | TryExcept _       -> "TryExcept"
  | Expr _            -> "Expr"
  | Pass _            -> "Pass"
  | Break _           -> "Break"
  | Continue _        -> "Continue"

and name_of_expr = function
  | BoolOp _       -> "BoolOp"
  | BinOp _        -> "BinOp"
  | UnaryOp _      -> "UnaryOp"
  | IfExp _        -> "IfExp"
  | Compare _      -> "Compare"
  | Call _         -> "Call"
  | Attribute _    -> "Attribute"
  | List _         -> "List"
  | Tuple _        -> "Tuple"
  | Num _          -> "Num"
  | Str _          -> "Str"
  | Bool _         -> "Bool"
  | Name _         -> "Name"

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

let annot_of_mod = function
  | Module (_, a)
    -> a

and annot_of_stmt = function
  | FunctionDef (_, _, _, a)
  | Return (_, a)
  | Assign (_, _, a)
  | Print (_, _, _, a)
  | While (_, _, a)
  | If (_, _, _, a)
  | Raise (_, _, a)
  | TryExcept (_, _, a)
  | Expr (_, a)
  | Pass (a)
  | Break (a)
  | Continue (a)
    -> a

and annot_of_expr = function
  | BoolOp (_, _, a)
  | BinOp (_, _, _, a)
  | UnaryOp (_, _, a)
  | IfExp (_, _, _, a)
  | Compare (_, _, _, a)
  | Call (_, _, a)
  | Attribute (_, _, a)
  | List (_, a)
  | Tuple (_, a)
  | Num (_, a)
  | Str (_, a)
  | Bool (_, a)
  | Name (_, a)
    -> a

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
