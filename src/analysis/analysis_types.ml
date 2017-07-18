type uid = Python2_ast_types.uid
[@@deriving eq, ord, show]
;;

type identifier = Python2_ast_types.identifier
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

type sign =
  | Pos
  | Neg
  | Zero
[@@deriving eq, ord, show]
;;

type abstract_str =
  | String_exact of string
  | String_lossy
[@@deriving eq, ord, show]
;;

type 'a abstract_list =
  | List_exact of 'a list
  | List_lossy of 'a list
[@@deriving eq, ord, show]
;;

type value_expression =
    | Integer_literal of sign
  | String_literal of abstract_str
  | Boolean_literal of bool
  | List_value of memory_variable abstract_list
  | Function_expression of value_variable abstract_list * block
  | None_literal
  | Empty_binding
[@@deriving eq, ord, show]

and binary_operator =
    | Binop_intplus
  | Binop_intminus
  | Binop_haskey
  | Binop_listconcat
  | Binop_equals
[@@deriving eq, ord, show]

and unary_operator =
    | Unop_not
  | Unop_is_function
  | Unop_is_int
[@@deriving eq, ord, show]

and directive =
    | Let_expression of value_variable * value_expression
  | Let_alloc of memory_variable
  | Let_alias_value of value_variable * value_variable
  | Let_alias_memory of memory_variable * memory_variable
  | Let_binding_update of value_variable * value_variable * value_variable * memory_variable
  | Let_binding_access of memory_variable * value_variable * value_variable
  | Let_list_access of memory_variable * value_variable * value_variable
  | Let_list_slice of value_variable * value_variable * value_variable * value_variable
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

type value =
  | Integer of sign
  | String of abstract_str
  | Boolean of bool
  | List of memory_variable abstract_list
  | Function of value_variable abstract_list * block
  | None_value
  | Empty_binding_value
[@@deriving eq, ord, show]
;;

type memory_location =
    | Memloc of statement
[@@deriving eq, ord, show]
;;
