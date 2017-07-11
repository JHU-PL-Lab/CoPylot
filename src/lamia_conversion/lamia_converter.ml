open Batteries;;
open Lamia_ast;;
open Python2_normalized_ast;;
open Lamia_conversion_ctx;;
open Lamia_conversion_utils;;

let rec convert_module
    (ctx : conversion_context)
    (m : 'a modl)
  : block =
  let Module(stmts, _) = m in
  (* TODO: Preamble, scope setup, etc *)
  Block(map_and_concat (convert_stmt ctx) stmts)

and convert_stmt
    (ctx : conversion_context)
    (s : 'a stmt)
  : statement list =
  (* let convert_stmt = convert_stmt ctx in *)
  let annotate_directive annot d = annotate_directive ctx annot d in
  match s with
  | Assign (id, value, annot) ->
    let value_bindings, value_result = convert_expr ctx value in
    let scope_update_directives =
      let varname = Value_variable(gen_unique_name ctx annot) in
      let old_scopeval = Value_variable(gen_unique_name ctx annot) in
      let new_scopeval = Value_variable(gen_unique_name ctx annot) in
      [
        Let_expression(varname, String_literal id);
        Let_get(old_scopeval, python_scope);
        Let_binding_update(new_scopeval, old_scopeval, varname, value_result);
        Store(python_scope, new_scopeval);
      ]
    in
    let scope_update_stmts =
      List.map (annotate_directive annot) scope_update_directives
    in
    value_bindings @ scope_update_stmts

  | Return (x, annot) ->
    let lookup_bindings, lookup_result = lookup ctx annot x in
    lookup_bindings @
    [
      annotate_directive annot @@ Lamia_ast.Return(lookup_result);
    ]


  | _ ->
    raise @@ Jhupllib_utils.Not_yet_implemented "Convert_stmt"

and convert_expr
    (ctx : conversion_context)
    (s : 'a expr)
  : statement list * memory_variable =
  ignore ctx; ignore s; failwith "NYI: convert_expr"
