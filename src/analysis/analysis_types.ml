open Batteries;;
open Jhupllib;;

type uid = Python2_ast_types.uid
[@@deriving eq, ord, show, to_yojson]
;;

type identifier = Python2_ast_types.identifier
[@@deriving eq, ord, show, to_yojson]
;;

type value_variable =
  | Value_variable of identifier
[@@deriving eq, ord, show, to_yojson]
;;

type memory_variable =
  | Memory_variable of identifier
[@@deriving eq, ord, show, to_yojson]
;;

type sign =
  | Pos
  | Neg
  | Zero
[@@deriving eq, ord, show, to_yojson]
;;

type abstract_str =
  | String_exact of string
  | String_lossy
[@@deriving eq, ord, show, to_yojson]
;;

type value_expression =
  | Integer_literal of sign
  | String_literal of abstract_str
  | Boolean_literal of bool
  | List_expression of memory_variable list
  | Function_expression of value_variable list * block
  | None_literal
  | Empty_binding
[@@deriving eq, ord, show, to_yojson]

and binary_operator =
    | Binop_intplus
  | Binop_intminus
  | Binop_haskey
  | Binop_listconcat
  | Binop_equals
[@@deriving eq, ord, show, to_yojson]

and unary_operator =
    | Unop_not
  | Unop_is_function
  | Unop_is_int
[@@deriving eq, ord, show, to_yojson]

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
[@@deriving eq, ord, show, to_yojson]

and statement =
    | Statement of uid * directive
[@@deriving eq, ord, show, to_yojson]

and block =
    | Block of statement list
[@@deriving eq, ord, show, to_yojson]
;;

type memory_location =
  | Memloc of statement
[@@deriving eq, ord, show, to_yojson]
;;

type abstract_memloc_list =
  | List_exact of memory_location list
  | List_lossy of memory_location list
[@@deriving eq, ord, show, to_yojson]
;;

module PpString =
struct
  type t = abstract_str
  [@@deriving eq,ord,show,to_yojson]
  ;;
end;;
module RawStringMap = Map.Make(PpString);;

module AbstractStringMap :
sig
  include module type of RawStringMap;;
  val pp : 'a Pp_utils.pretty_printer ->
    'a RawStringMap.t Pp_utils.pretty_printer;;
  val to_yojson : ('v ->  Yojson.Safe.json) -> 'v RawStringMap.t -> Yojson.Safe.json;;
end =
struct
  include RawStringMap;;
  include Pp_utils.Map_pp(RawStringMap)(PpString);;
  include Yojson_utils.Map_to_yojson(RawStringMap)(PpString);;
end;;

type value =
  | Integer_value of sign
  | String_value of abstract_str
  | Boolean_value of bool
  | List_value of abstract_memloc_list
  | Object_value of memory_location AbstractStringMap.t
  | Function_value of value_variable list * block
  | None_value
[@@deriving eq, ord, show, to_yojson]
;;
