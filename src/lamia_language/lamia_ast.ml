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

type value_expression =
  | Integer_literal of int
  | String_literal of string
  | Boolean_literal of bool
  | List_value of memory_variable list
  | Tuple_value of memory_variable list
  | Function_expression of value_variable list * block
  | None_literal
  | Empty_binding
[@@deriving eq, ord, show]

and binary_operator =
    | Binop_intplus
  | Binop_intminus
  | Binop_and
  | Binop_or
  | Binop_haskey
[@@deriving eq, ord, show]

and unary_operator =
    | Unop_not
[@@deriving eq, ord, show]

and directive =
    | Let_expression of value_variable * value_expression
  | Let_alloc of memory_variable
  | Let_alias_value of value_variable * value_variable
  | Let_alias_memory of memory_variable * memory_variable
  | Let_binding_update of value_variable * value_variable * value_variable * memory_variable
  | Let_binding_access of memory_variable * value_variable * value_variable
  | Let_call_function of memory_variable * value_variable * value_variable list
  | Store of memory_variable * value_variable
  | Let_get of value_variable * memory_variable
  | Let_is of value_variable * memory_variable * memory_variable
  | Let_unop of value_variable * unary_operator * value_variable
  | Let_binop of value_variable * value_variable * binary_operator * value_variable
  | Return of memory_variable
  | If_result_value of value_variable
  | If_result_memory of memory_variable
  | Raise of memory_variable
  | Try_except of block * memory_variable * block
  | Let_conditional_value of value_variable * value_variable * block * block
  | Let_conditional_memory of memory_variable * value_variable * block * block
  | While of memory_variable * block
[@@deriving eq, ord, show]

and statement =
    | Statement of uid * directive
[@@deriving eq, ord, show]

and block =
    | Block of statement list
[@@deriving eq, ord, show]
;;
