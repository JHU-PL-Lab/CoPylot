open Batteries;;

type uid = Python2_ast_types.uid
[@@deriving eq, ord, show]
;;

type identifier = Python2_ast_types.identifier
[@@deriving eq, ord, show]
;;

type number = Python2_ast_types.number
[@@deriving eq, ord, show]
;;

type value_variable =
  | Value_variable of identifier
[@@deriving eq, ord, show]
;;

type memory_variable =
  | Memory_variable of identifier
[@@deriving eq, ord, show]
;;

type binary_operator =
  | Binop_intplus
  | Binop_intminus
  | Binop_haskey
  | Binop_listconcat
  | Binop_equals
[@@deriving eq, ord, show]
;;

type unary_operator =
  | Unop_not
  | Unop_is_function
  | Unop_is_int
[@@deriving eq, ord, show]
;;
