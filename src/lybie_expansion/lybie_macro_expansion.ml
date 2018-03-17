open Batteries;;
open Lybie_ast;;
open Unique_name_ctx;;

open Lybie_conversion_builtin_names;;
(* open Lybie_conversion_builtin_defs;; *)

open Lybie_conversion_monad;;
open Lybie_monad;;

let expand_macro
    (ctx : name_context)
    (annot : 'a)
    (m : macro)
  : 'a statement list =
  snd @@
  run ctx annot @@
  local_annot annot @@
  match m with
  (*
| Python_function_preamble ->
    let%bind empty_scope = fresh_value_var () in
    let%bind get_from_scope_val = get_from_scope_def in
    let%bind _ =
      emit
        [
          Let_alias_value(get_from_parent_scope, get_from_scope);
          Let_alloc(python_scope);
          Let_expression(get_from_scope, get_from_scope_val);
          Let_expression(empty_scope, Empty_binding);
        ] *)
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
