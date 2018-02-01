open Batteries;;
module Lamia = Lamia_ast;;
module Macros = Lamia_macro_ast;;

let rec expand_macros_block (m : 'a Macros.block) : 'a Lamia.block =
  let Macros.Block(stmts) = m in
  Lamia.Block(List.concat @@ List.map expand_macros_stmt stmts)

and expand_macros_stmt
    (stmt : 'a Macros.statement)
  : 'a Lamia.statement list =
  match stmt with
  | Macros.Lamia_statement (annot, d) ->
    [Lamia.Statement(annot, d)]
  | Macros.Stmt_Macro(macro) ->
    expand_macro macro

and expand_macro
    (m : 'a Macros.stmt_macro)
  : 'a Lamia.statement list =
  match m with
  | Macros.TODO _ -> failwith "Don't have any macros yet"
;;
