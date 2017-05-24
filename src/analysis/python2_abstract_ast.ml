

type identifier = string
[@@deriving eq, ord, show]

and 'a modl =
   | Module of 'a stmt list (* body *) * 'a
(* | Interactive of 'a stmt list (* body *) * 'a *)
(* | Expression of 'a expr (* body *) * 'a *)

(* | Suite of 'a stmt list (* body *) * 'a *)
[@@deriving eq, ord, show]

and 'a stmt =
  | FunctionDef of identifier (* name *) * 'a arguments (* args *) * 'a stmt list (* body *) * 'a expr list (* decorator_list *) * 'a
  (* | ClassDef of identifier (* name *) * 'a expr list (* bases *) * 'a stmt list (* body *) * 'a expr list (* decorator_list *) * 'a *)
  | Return of 'a expr option (* value *) * 'a

  (* | Delete of 'a expr list (* targets *) * 'a *)
  | Assign of 'a expr list (* targets *) * 'a expr (* value *) * 'a
  | AugAssign of 'a expr (* target *) * operator (* op *) * 'a expr (* value *) * 'a

  | Print of 'a expr option (* dest *) * 'a expr list (* values *) * bool (* nl *) * 'a

  | For of 'a expr (* target *) * 'a expr (* iter *) * 'a stmt list (* body *) * 'a stmt list (* orelse *) * 'a
  | While of 'a expr (* test *) * 'a stmt list (* body *) * 'a stmt list (* orelse *) * 'a
  | If of 'a expr (* test *) * 'a stmt list (* body *) * 'a stmt list (* orelse *) * 'a
  (* | With of 'a expr (* context_expr *) * 'a expr option (* optional_vars *) * 'a stmt list (* body *) * 'a *)

  (* | Raise of 'a expr option (* type *) * 'a expr option (* inst *) * 'a expr option (* tback *) * 'a *)
  (* | TryExcept of 'a stmt list (* body *) * 'a excepthandler list (* handlers *) * 'a stmt list (* orelse *) * 'a *)
  (* | TryFinally of 'a stmt list (* body *) * 'a stmt list (* finalbody *) * 'a *)
  (* | Assert of 'a expr (* test *) * 'a expr option (* msg *) * 'a *)

  (* | Import of alias list (* names *) * 'a *)
  (* | ImportFrom of identifier (* module *) * alias list (* names *) * int option (* level *) * 'a *)

  (* | Exec of 'a expr (* body *) * 'a expr option (* globals *) * 'a expr option (* locals *) * 'a *)

  (* | Global of identifier list (* names *) * 'a *)
  | Expr of 'a expr (* value *) * 'a
  | Pass of 'a
  | Break of 'a
  | Continue of 'a
[@@deriving eq, ord, show]

and 'a expr =
  | BoolOp of boolop (* op *) * 'a expr list (* values *) * 'a
  | BinOp of 'a expr (* left *) * operator (* op *) * 'a expr (* right *) * 'a
  | UnaryOp of unaryop (* op *) * 'a expr (* operand *) * 'a
  (* | Lambda of 'a arguments (* args *) * 'a expr (* body *) * 'a *)
  | IfExp of 'a expr (* test *) * 'a expr (* body *) * 'a expr (* orelse *) * 'a
  (* | Dict of 'a expr list (* keys *) * 'a expr list (* values *) * 'a *)
  (* | ListComp of 'a expr (* elt *) * 'a comprehension list (* generators *) * 'a *)
  (* | GeneratorExp of 'a expr (* elt *) * 'a comprehension list (* generators *) * 'a *)
  (* | Yield of 'a expr option (* value *) * 'a *)
  | Compare of 'a expr (* left *) * cmpop list (* ops *) * 'a expr list (* comparators *) * 'a
  | Call of 'a expr (* func *) * 'a expr list (* args *) * 'a keyword list (* keywords *) * 'a expr option (* starargs *) * 'a expr option (* kwargs *) * 'a
  (* | Repr of 'a expr (* value *) * 'a *)
  | Num of number (* n *) * 'a
  | Str of 'a
  | Bool of bool * 'a (* N.B. The Python formal specification does not treat Bools
                         as a type of expression. We are intentionally deviating
                         from this behavior here *)

  (* | Attribute of 'a expr (* value *) * identifier (* attr *) * expr_context (* ctx *) * 'a *)
  | Subscript of 'a expr (* value *) * 'a slice (* slice *) * expr_context (* ctx *) * 'a
  | Name of identifier (* id *) * expr_context (* ctx *) * 'a
  | List of 'a expr list (* elts *) * expr_context (* ctx *) * 'a
  | Tuple of 'a expr list (* elts *) * expr_context (* ctx *) * 'a
[@@deriving eq, ord, show]

(* AugLoad and AugStore are not used *)
and expr_context = Load | Store | Del | AugLoad | AugStore | Param
[@@deriving eq, ord, show]

and 'a slice =
  (* | Ellipsis *)
  | Slice of 'a expr option (* lower *) * 'a expr option (* upper *) * 'a expr option (* step *)
  (* | ExtSlice of 'a slice list (* dims *) *)
  | Index of 'a expr (* value *)
[@@deriving eq, ord, show]

and boolop = And | Or
[@@deriving eq, ord, show]

and operator = Add | Sub | Mult | Div | Mod | Pow
(* | LShift | RShift | BitOr | BitXor | BitAnd | FloorDiv *)
[@@deriving eq, ord, show]

and unaryop = (*Invert |*) Not | UAdd | USub
[@@deriving eq, ord, show]

and cmpop = Eq | NotEq | Lt | LtE | Gt | GtE (*| Is | IsNot*) | In | NotIn
[@@deriving eq, ord, show]

(* and 'a comprehension = 'a expr (* target *) * 'a expr (* iter *) * 'a expr list (* ifs *) *)
(* [@@deriving eq, ord, show] *)

(* and 'a excepthandler = ExceptHandler of 'a expr option (* type *) * 'a expr option (* name *) * 'a stmt list (* body *) * 'a *)
(* [@@deriving eq, ord, show] *)

and 'a arguments = 'a expr list (* args *) * identifier option (* varargs *) * identifier option (* kwargs *) * 'a expr list (* defaults *)
[@@deriving eq, ord, show]

and 'a keyword = identifier (* arg *) * 'a expr (* value *)
[@@deriving eq, ord, show]

(* and alias = identifier (* name *) * identifier option (* asname *)
   [@@deriving eq, ord, show] *)

and sign = Pos | Neg | Zero
[@@deriving eq, ord, show]

and number =
  | Int of sign
  | Float of sign
  (*
  | Imag of string*)
[@@deriving eq, ord, show]
