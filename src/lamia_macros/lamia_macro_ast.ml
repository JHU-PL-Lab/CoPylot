open Batteries;;
(* open Lamia_ast;; *)

type 'a statement =
    | Lamia_statement of 'a * 'a Lamia_ast.directive
  | Stmt_Macro of 'a stmt_macro
[@@deriving eq, ord, show]

and 'a stmt_macro =
    | TODO of 'a
[@@deriving eq, ord, show]

and 'a block =
    | Block of 'a statement list
[@@deriving eq, ord, show]
;;
