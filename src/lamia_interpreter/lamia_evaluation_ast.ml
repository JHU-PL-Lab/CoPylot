open Batteries;;

type uid = Lamia_ast.uid;;
let equal_uid = Lamia_ast.equal_uid;;
let compare_uid = Lamia_ast.compare_uid;;
let pp_uid = Lamia_ast.pp_uid;;
let show_uid = Lamia_ast.show_uid;;
module Uid =
struct
  type t = Lamia_ast.uid;;
  let equal = equal_uid;;
  let compare = compare_uid;;
  let pp = pp_uid;;
  let show = show_uid;;
end;;

type identifier = Lamia_ast.identifier;;
let equal_identifier = Lamia_ast.equal_identifier;;
let compare_identifier = Lamia_ast.compare_identifier;;
let pp_identifier = Lamia_ast.pp_identifier;;
let show_identifier = Lamia_ast.show_identifier;;
module Identifier =
struct
  type t = Lamia_ast.identifier;;
  let equal = equal_identifier;;
  let compare = compare_identifier;;
  let pp = pp_identifier;;
  let show = show_identifier;;
end;;

type number = Lamia_ast.number;;
let equal_number = Lamia_ast.equal_number;;
let compare_number = Lamia_ast.compare_number;;
let pp_number = Lamia_ast.pp_number;;
let show_number = Lamia_ast.show_number;;
module Number =
struct
  type t = Lamia_ast.number;;
  let equal = equal_number;;
  let compare = compare_number;;
  let pp = pp_number;;
  let show = show_number;;
end;;

type value_variable = Lamia_ast.value_variable;;
let equal_value_variable = Lamia_ast.equal_value_variable;;
let compare_value_variable = Lamia_ast.compare_value_variable;;
let pp_value_variable = Lamia_ast.pp_value_variable;;
let show_value_variable = Lamia_ast.show_value_variable;;
module Value_variable =
struct
  type t = Lamia_ast.value_variable;;
  let equal = equal_value_variable;;
  let compare = compare_value_variable;;
  let pp = pp_value_variable;;
  let show = show_value_variable;;
end;;

type memory_variable = Lamia_ast.memory_variable;;
let equal_memory_variable = Lamia_ast.equal_memory_variable;;
let compare_memory_variable = Lamia_ast.compare_memory_variable;;
let pp_memory_variable = Lamia_ast.pp_memory_variable;;
let show_memory_variable = Lamia_ast.show_memory_variable;;
module Memory_variable =
struct
  type t = Lamia_ast.memory_variable;;
  let equal = equal_memory_variable;;
  let compare = compare_memory_variable;;
  let pp = pp_memory_variable;;
  let show = show_memory_variable;;
end;;

type value_expression =
  | Integer_literal of int
  | String_literal of string
  | Boolean_literal of bool
  | List_value of memory_variable list
  | Function_expression of value_variable list * block
  | None_literal
  | Empty_binding
[@@deriving eq, ord, show]

and binary_operator = Lamia_ast.binary_operator

and unary_operator = Lamia_ast.unary_operator

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
    | Statement of directive
  | Active_while_block of block
  | Active_if_value_block of value_variable * block
  | Active_if_memory_block of memory_variable * block
  | Active_fun_block of memory_variable * block
[@@deriving eq, ord, show]

and block =
    | Block of statement list
[@@deriving eq, ord, show]
;;
