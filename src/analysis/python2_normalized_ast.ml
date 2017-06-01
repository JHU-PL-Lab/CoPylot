type uid = int
[@@deriving eq, ord, show]
;;

type identifier = string
[@@deriving eq, ord, show]

and modl =
    | Module of stmt list (* body *) * uid
[@@deriving eq, ord, show]

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
[@@deriving eq, ord, show]

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
[@@deriving eq, ord, show]

and simple_expr =
    | Num of number (* n *) * uid * uid option (* exception label *)
  | Str of str * uid * uid option (* exception label *)
  | Bool of bool * uid * uid option (* exception label *)
  | Name of identifier (* id *) * uid * uid option (* exception label *)
[@@deriving eq, ord, show]

and boolop = And | Or
[@@deriving eq, ord, show]

and operator = Add | Sub | Mult | Div | Mod | Pow
[@@deriving eq, ord, show]

and unaryop = Not | UAdd | USub
[@@deriving eq, ord, show]

and cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | In | NotIn
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

(*
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
  | Compare _      -> "Compare"
  | Call _         -> "Call"
  | Attribute _    -> "Attribute"
  | List _         -> "List"
  | Tuple _        -> "Tuple"
  | SimpleExpr _   -> "SimpleExpr"

and name_of_simple_expr = function
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

let uid_of_mod = function
  | Module (_, u)
    -> u

and uid_of_stmt = function
  | FunctionDef (_, _, _, u)
  | Return (_, u)
  | Assign (_, _, u)
  | Print (_, _, _, u)
  | If (_, _, _, u)
  | Raise (_, u)
  | TryExcept (_, _, u)
  | SimpleExprStmt (_, u)
  | Pass (u)
  | Goto (_, u)
    -> u

and uid_of_compound_expr = function
  | BoolOp (_, _, _, u)
  | BinOp (_, _, _, u)
  | UnaryOp (_, _, u)
  | Compare (_, _, _, u)
  | Call (_, _, u)
  | Attribute (_, _, u)
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
*)
