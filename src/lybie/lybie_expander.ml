open Batteries;;
module Lamia = Lamia_ast;;
module Macros = Lybie_ast;;
(* TODO: Probably gonna want lamia_conversion_monad *)

let rec expand_macros_block (m : 'a Macros.block) : 'a Lamia.block =
  let Macros.Block(stmts) = m in
  Lamia.Block(List.concat @@ List.map expand_macros_stmt stmts)

and expand_macros_stmt
    (stmt : 'a Macros.statement)
  : 'a Lamia.statement list =
  match stmt with
  | Macros.Lamia_statement (annot, d) ->
    [Lamia.Statement(annot, d)]
  | Macros.Stmt_Macro(annot, macro) ->
    expand_macro annot macro

and expand_macro
    (annot : 'a)
    (m : Macros.stmt_macro)
  : 'a Lamia.statement list =
  ignore @@ annot;
  match m with
  | Macros.Python_function_preamble(args) ->
    ignore args; failwith "NYI"
;;
