open Batteries;;
open Lybie_ast;;
open Unique_name_ctx;;

open Lamia_conversion_builtin_names;;

open Lybie_conversion_monad;;
open Lybie_monad;;

let expand_macro
    (ctx : name_context)
    (annot : 'a)
    (m : stmt_macro)
  : 'a statement list =
  snd @@
  run ctx annot @@
  local_annot annot @@
  match m with
  | Assign_python_variable(id, y) ->
    let%bind varname = fresh_value_var () in
    let%bind old_scopeval = fresh_value_var () in
    let%bind new_scopeval = fresh_value_var () in
    emit
      [
        Let_expression(varname, String_literal id);
        Let_get(old_scopeval, python_scope);
        Let_binding_update(new_scopeval, old_scopeval, varname, y);
        Store(python_scope, new_scopeval);
      ]
;;
