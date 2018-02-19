open Batteries;;
open Lamia_ast_types;;

type 'a value_expression =
  | Integer_literal of int
  | String_literal of string
  | Boolean_literal of bool
  | List_expression of memory_variable list
  | Function_expression of value_variable list * 'a block
  | None_literal
  | Empty_binding
[@@deriving eq, ord, show]

and 'a directive =
    | Let_expression of value_variable * 'a value_expression
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
  | Try_except of 'a block * memory_variable * 'a block
  | Let_conditional_value of value_variable * value_variable * 'a block * 'a block
  | Let_conditional_memory of memory_variable * value_variable * 'a block * 'a block
  | While of memory_variable * 'a block
[@@deriving eq, ord, show]

and 'a statement =
    | Statement of 'a * 'a directive
  | Stmt_macro of 'a * stmt_macro
[@@deriving eq, ord, show]

and stmt_macro =
    (* For the moment we only handle functions with constant arg number *)
    (* Unpacks the arguments to a lamia function which is modeling a python
       function *)
    | Unpack_python_args of identifier list (* args *)
[@@deriving eq, ord, show]

and 'a block =
    | Block of 'a statement list
[@@deriving eq, ord, show]
;;
