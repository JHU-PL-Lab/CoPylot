(*
 * This ast is a pure Python2 ast, except:
 *  * We add magic names for special builtin values
 *  * We remove information we don't care about, such as load contexts
 *)

open Python2_ast_types;;

type annotated_modl = modl annotation
[@@deriving eq, ord, show, to_yojson]

and annotated_stmt = stmt annotation
[@@deriving eq, ord, show, to_yojson]

and annotated_expr = expr annotation
[@@deriving eq, ord, show, to_yojson]

and modl =
    | Module of annotated_stmt list (* body *)
[@@deriving eq, ord, show]

and stmt =
    | FunctionDef of identifier (* name *) * identifier list (* args *) * annotated_stmt list (* body *)
  | Return of annotated_expr option (* value *)
  | Assign of annotated_expr list (* targets *) * annotated_expr (* value *)
  | AugAssign of annotated_expr (* target *) * operator (* op *) * annotated_expr (* value *)
  | For of annotated_expr (* target *) * annotated_expr (* iter *) * annotated_stmt list (* body *) * annotated_stmt list (* orelse *)
  | While of annotated_expr (* test *) * annotated_stmt list (* body *) * annotated_stmt list (* orelse *)
  | If of annotated_expr (* test *) * annotated_stmt list (* body *) * annotated_stmt list (* orelse *)
  | Raise of annotated_expr option (* type *)
  | TryExcept of annotated_stmt list (* body *) * excepthandler list (* handlers *) * annotated_stmt list (* orelse *)
  | Expr of annotated_expr (* value *)
  | Pass
  | Break
  | Continue
[@@deriving eq, ord, show]

and expr =
    | BoolOp of boolop (* op *) * annotated_expr list (* values *)
  | BinOp of annotated_expr (* left *) * operator (* op *) * annotated_expr (* right *)
  | UnaryOp of unaryop (* op *) * annotated_expr (* operand *)
  | IfExp of annotated_expr (* test *) * annotated_expr (* body *) * annotated_expr (* orelse *)
  | Compare of annotated_expr (* left *) * cmpop list (* ops *) * annotated_expr list (* comparators *)
  | Call of annotated_expr (* func *) * annotated_expr list (* args *)
  | Num of number (* n *)
  | Str of string (* s *)
  | Bool of bool (* N.B. The Python formal specification does not treat Bools
                         as a type of annotated_expression. We are intentionally deviating
                         from this behavior here *)
  | Attribute of annotated_expr (* value *) * identifier (* attr *)
  | Subscript of annotated_expr (* value *) * slice (* slice *)
  | Name of identifier (* id *)
  | List of annotated_expr list (* elts *)
  | Tuple of annotated_expr list (* elts *)
  | Builtin of builtin
  | NoneExpr
[@@deriving eq, ord, show]

and slice =
    | Slice of annotated_expr option (* lower *) * annotated_expr option (* upper *) * annotated_expr option (* step *)
  | Index of annotated_expr (* value *)
[@@deriving eq, ord, show]

and boolop = And | Or
[@@deriving eq, ord, show]

and operator = Add | Sub | Mult | Div | Mod | Pow
[@@deriving eq, ord, show]

and unaryop = Not | UAdd | USub
[@@deriving eq, ord, show]

and cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn
[@@deriving eq, ord, show]

and excepthandler = ExceptHandler of annotated_expr option (* type *) * annotated_expr option (* name *) * annotated_stmt list (* body *)
[@@deriving eq, ord, show]
