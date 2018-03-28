(*
 * This ast is a pure Python2 ast, except:
 *  * We add magic names for special builtin values
 *  * We remove information we don't care about, such as load contexts
 *)

open Python2_ast_types;;

type 'a modl =
    | Module of 'a stmt list (* body *) * 'a
[@@deriving eq, ord, show]

and 'a stmt =
    | FunctionDef of identifier (* name *) * identifier list (* args *) * 'a stmt list (* body *) * 'a
  | Return of 'a expr option (* value *) * 'a
  | Assign of 'a expr list (* targets *) * 'a expr (* value *) * 'a
  | AugAssign of 'a expr (* target *) * operator (* op *) * 'a expr (* value *) * 'a
  | Print of 'a expr option (* dest *) * 'a expr list (* values *) * bool (* nl *) * 'a
  | For of 'a expr (* target *) * 'a expr (* iter *) * 'a stmt list (* body *) * 'a stmt list (* orelse *) * 'a
  | While of 'a expr (* test *) * 'a stmt list (* body *) * 'a stmt list (* orelse *) * 'a
  | If of 'a expr (* test *) * 'a stmt list (* body *) * 'a stmt list (* orelse *) * 'a
  | Raise of 'a expr option (* type *) * 'a
  | TryExcept of 'a stmt list (* body *) * 'a excepthandler list (* handlers *) * 'a stmt list (* orelse *) * 'a
  | Expr of 'a expr (* value *) * 'a
  | Pass of 'a
  | Break of 'a
  | Continue of 'a
[@@deriving eq, ord, show]

and 'a expr =
    | BoolOp of boolop (* op *) * 'a expr list (* values *) * 'a
  | BinOp of 'a expr (* left *) * operator (* op *) * 'a expr (* right *) * 'a
  | UnaryOp of unaryop (* op *) * 'a expr (* operand *) * 'a
  | IfExp of 'a expr (* test *) * 'a expr (* body *) * 'a expr (* orelse *) * 'a
  | Compare of 'a expr (* left *) * cmpop list (* ops *) * 'a expr list (* comparators *) * 'a
  | Call of 'a expr (* func *) * 'a expr list (* args *) * 'a
  | Num of number (* n *) * 'a
  | Str of string (* s *) * 'a
  | Bool of bool * 'a (* N.B. The Python formal specification does not treat Bools
                         as a type of expression. We are intentionally deviating
                         from this behavior here *)
  | Attribute of 'a expr (* value *) * identifier (* attr *) * 'a
  | Subscript of 'a expr (* value *) * 'a slice (* slice *) * 'a
  | Name of identifier (* id *) * 'a
  | List of 'a expr list (* elts *) * 'a
  | Tuple of 'a expr list (* elts *) * 'a
  | Builtin of builtin * 'a
  | NoneExpr of 'a
[@@deriving eq, ord, show]

and 'a slice =
    | Slice of 'a expr option (* lower *) * 'a expr option (* upper *) * 'a expr option (* step *)
  | Index of 'a expr (* value *)
[@@deriving eq, ord, show]

and boolop = And | Or
[@@deriving eq, ord, show]

and operator = Add | Sub | Mult | Div | Mod | Pow
[@@deriving eq, ord, show]

and unaryop = Not | UAdd | USub
[@@deriving eq, ord, show]

and cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn
[@@deriving eq, ord, show]

and 'a excepthandler = ExceptHandler of 'a expr option (* type *) * 'a expr option (* name *) * 'a stmt list (* body *) * 'a
[@@deriving eq, ord, show]

and 'a keyword = identifier (* arg *) * 'a expr (* value *)
[@@deriving eq, ord, show]

and number =
    | Int of int
  | Float of float
[@@deriving eq, ord, show]
