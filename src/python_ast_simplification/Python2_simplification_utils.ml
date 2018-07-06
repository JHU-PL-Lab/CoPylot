open Batteries;;
open Python2_ast_types;;
open Python_simplification_monad;;
module Augmented = Python2_augmented_ast;;
module Simplified = Python2_simplified_ast;;

(* Add a "return None" if the function might not explicitly Return
   otherwise *)
let add_return (stmts : Augmented.annotated_stmt list) annot =
  let rec needs_return (stmts : Augmented.annotated_stmt list) =
    (* Returns true iff we can't guarantee that the function will explicitly
       return. This implementation is not super efficient or important;
       it would also be acceptable to just always return true
       in which case we'd generate dead code sometimes. Oh well. *)
    match stmts with
    | [] -> true
    | hd::tl ->
      match hd.body with
      | Augmented.Return _ -> false
      (* If both branches of the if end in a return, we're good! *)
      | Augmented.If(_, body, orelse) ->
        if (needs_return body) || (needs_return orelse) then
          needs_return tl
        else
          false
      | _ -> needs_return tl
  in
  if needs_return stmts then
    stmts @ [annotate annot @@ Augmented.Return(Some(annotate annot @@ Augmented.NoneExpr))]
  else
    stmts
;;

let simplify_list simp_func lst =
  Simplification_monad.sequence @@ List.map simp_func lst
;;
