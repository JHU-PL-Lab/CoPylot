open Python2_ast_types;;

type 'a modl =
  | Module of 'a stmt list (* body *) * 'a
[@@deriving eq, ord, show]

and 'a stmt =
    | Assign of identifier (* target *) * 'a expr (* value *) * 'a
  | Return of 'a expr (* value *) * 'a
(* We maintain an invariant that the test statement of a while loop is always
   an actual boolean value. We also require that the test be an identifer, so
   that there is no more work to be done during normalization. This lets us
   ensure that we can append all necessary computation of the test value to the
   body of the while during simplification. *)
  | While of identifier (* test *) * 'a stmt list (* body *) * 'a
  (* We maintain the same invariant for if statements as for while loops *)
  | If of 'a expr (* test *) * 'a stmt list (* body *) * 'a stmt list (* orelse *) * 'a
  (* Raise is very complicated, with different behaviors based on the
       number of arguments it recieves. For simplicity we require that
       it take exactly one argument, which is the value to be raised. *)
  | Raise of 'a expr (* value *) * 'a
  | TryExcept of 'a stmt list (* body *) * identifier (* exn name *) * 'a stmt list (* handlers *) * 'a
  | Pass of 'a
  | Break of 'a
  | Continue of 'a
  | Expr of 'a expr (* value *) * 'a
[@@deriving eq, ord, show]

and 'a expr =
    | UnaryOp of unaryop (* op *) * 'a expr (* value *) * 'a
  | Binop of 'a expr (* right *) * binop (* op *) * 'a expr (* right *) * 'a
  | Call of 'a expr (* func *) * 'a expr list (* args *) * 'a
  | Attribute of 'a expr (* object *) * string (* attr *) * 'a
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

let extract_stmt_annot = function
  | Assign (_, _, a)
  | Return(_,a)
  | While(_,_,a)
  | If(_,_,_,a)
  | Raise(_,a)
  | TryExcept(_,_,_,a)
  | Pass(a)
  | Break(a)
  | Continue(a)
  | Expr(_,a)
    -> a

let extract_expr_annot = function
  | UnaryOp (_,_,a)
  | Binop (_,_,_,a)
  | Call(_,_,a)
  | Attribute(_,_,a)
  | List(_,a)
  | Tuple(_,a)
  | Num (_,a)
  | Str(_,a)
  | Bool(_,a)
  | Name(_,a)
  | Builtin(_,a)
  | FunctionVal (_,_,a)
    -> a
