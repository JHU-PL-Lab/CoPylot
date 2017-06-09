type uid = int
[@@deriving eq, ord, show, to_yojson]
;;

type identifier = string
[@@deriving eq, ord, show, to_yojson]

and 'a annotation =
  {
    uid: uid;
    exception_target: uid option;
    multi: bool;
    body: 'a;
  }
[@@deriving eq, ord, show, to_yojson]

and annotated_stmt = stmt annotation
[@@deriving eq, ord, show, to_yojson]

and annotated_cexpr = compound_expr annotation
[@@deriving eq, ord, show, to_yojson]

and annotated_sexpr = simple_expr annotation
[@@deriving eq, ord, show, to_yojson]

and modl =
    | Module of annotated_stmt list (* body *) * uid
[@@deriving eq, ord, show, to_yojson]

and stmt =
    | Assign of identifier (* target *) * annotated_cexpr (* value *)
  | FunctionDef of identifier (* name *) * identifier list (* args *) * annotated_stmt list (* body *)
  | Return of annotated_sexpr option (* value *)
  | Print of annotated_sexpr option (* dest *) * annotated_sexpr list (* values *) * bool (* nl *)
  | Raise of annotated_sexpr (* value *)
  | Catch of identifier (* name *)
  | Pass
  | Goto of uid (* dest *)
  | GotoIfNot of annotated_sexpr (* test *) * uid (* dest *) (* Jump iff test is falsey *)
  | SimpleExprStmt of annotated_sexpr (* value *)
[@@deriving eq, ord, show, to_yojson]

and compound_expr =
  | Call of annotated_sexpr (* func *) * annotated_sexpr list (* args *)
  | Attribute of annotated_sexpr (* object *) * identifier (* attr *)
  | List of annotated_sexpr list (* elts *)
  | Tuple of annotated_sexpr list (* elts *)
  | SimpleExpr of annotated_sexpr (* value*)
[@@deriving eq, ord, show, to_yojson]

and simple_expr =
    | Literal of literal
  | Name of identifier (* id *)
[@@deriving eq, ord, show, to_yojson]

and literal =
    | Num of number
  | Str of str
  | Bool of bool
  | Builtin of builtin
[@@deriving eq, ord, show, to_yojson]

and sign = Pos | Neg | Zero
[@@deriving eq, ord, show, to_yojson]

and number =
    | Int of sign
  | Float of sign
[@@deriving eq, ord, show, to_yojson]

and str =
    | StringAbstract
  | StringLiteral of string
[@@deriving eq, ord, show, to_yojson]

and builtin =
    | Builtin_slice
  | Builtin_bool
  | Builtin_type
[@@deriving eq, ord, show, to_yojson]
