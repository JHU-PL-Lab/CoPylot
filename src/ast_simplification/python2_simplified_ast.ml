open Python2_ast_types;;

type 'a modl =
  | Module of 'a stmt list (* body *) * 'a
[@@deriving eq, ord, show]

and 'a stmt =
    | Assign of identifier (* target *) * 'a expr (* value *) * 'a
  | Return of 'a expr (* value *) * 'a
  | Print of 'a expr option (* dest *) * 'a expr list (* values *) * bool (* nl *) * 'a
  (* We maintain the invariant that simplified while loops always have a boolean value as their test *)
  | While of identifier (* test *) * 'a stmt list (* body *) * 'a
  (* TODO Same invariant as while loops *)
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
  | UnaryOp of unaryop (* op *) * 'a expr (* value *) * 'a
  | Binop of 'a expr (* right *) * binop (* op *) * 'a expr (* right *) * 'a
  | Call of 'a expr (* func *) * 'a expr list (* args *) * 'a
  | Attribute of 'a expr (* object *) * string (* attr *) * 'a
  | SimpleAttribute of 'a expr (* object *) * string (* attr *) * 'a (* Double-dot operator *)
  | ImmediateAttribute of 'a expr (* object *) * identifier (* attr *) * 'a (* Triple-dot operator *)
  | List of 'a expr list (* elts *)  * 'a
  | Tuple of 'a expr list (* elts *)  * 'a
  | Num of number (* n *) * 'a
  | Str of string * 'a
  | Bool of bool * 'a
  | Name of identifier (* id *) * 'a
  | Builtin of builtin * 'a
  | FunctionVal of identifier list (* args *) * 'a stmt list (* body *) * 'a
[@@deriving eq, ord, show]

and boolop = And | Or
[@@deriving eq, ord, show]

and binop = Is
[@@deriving eq, ord, show]

and unaryop = Not
[@@deriving eq, ord, show]

and 'a excepthandler = ExceptHandler of 'a expr option (* type *) * identifier option (* name *) * 'a stmt list (* body *) * 'a
[@@deriving eq, ord, show]

let extract_stmt_annot = function
  | Assign (_, _, a)
  | Return(_,a)
  | Print(_,_,_,a)
  | While(_,_,a)
  | If(_,_,_,a)
  | Raise(_,a)
  | TryExcept(_,_,a)
  | Pass(a)
  | Break(a)
  | Continue(a)
  | Expr(_,a)
    -> a

let extract_expr_annot = function
  | BoolOp (_,_,a)
  | UnaryOp (_,_,a)
  | Binop (_,_,_,a)
  | Call(_,_,a)
  | Attribute(_,_,a)
  | SimpleAttribute(_,_,a)
  | ImmediateAttribute(_,_,a)
  | List(_,a)
  | Tuple(_,a)
  | Num (_,a)
  | Str(_,a)
  | Bool(_,a)
  | Name(_,a)
  | Builtin(_,a)
  | FunctionVal (_,_,a)
    -> a
