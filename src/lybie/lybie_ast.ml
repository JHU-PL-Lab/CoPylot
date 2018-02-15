open Batteries;;
open Lamia_ast;;

type 'a statement =
    | Lamia_statement of 'a * 'a Lamia_ast.directive
  | Stmt_Macro of 'a * stmt_macro
[@@deriving eq, ord, show]

and stmt_macro =
    (* For the moment we only handle functions with constant arg number *)
    | Python_function_preamble of identifier list (* args *)
[@@deriving eq, ord, show]

and 'a block =
    | Block of 'a statement list
[@@deriving eq, ord, show]
;;
