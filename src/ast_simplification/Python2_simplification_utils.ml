open Batteries;;
(* open Python2_ast_types;; *)
module Augmented = Python2_augmented_ast;;
module Simplified = Python2_simplified_ast;;

(* Add a "return None" if the function might not explicitly Return
   otherwise *)
let add_return stmts annot =
  let rec needs_return = function
    (* Returns true iff we can't guarantee that the function will explicitly
       return. This implementation is not super efficient or important;
       it would also be acceptable to just always return true
       in which case we'd generate dead code sometimes. Oh well. *)
    | [] -> true
    | [Augmented.Return _] -> false
    (* If both branches of the if end in a return, we're good! *)
    | Augmented.If(_,body,orelse,_)::rest ->
      if (needs_return body) || (needs_return orelse) then
        needs_return rest
      else
        false
    | _::tl -> needs_return tl
  in
  if needs_return stmts then
    stmts @ [Augmented.Return(Some(Augmented.NoneExpr annot), annot)]
  else
    stmts
;;

let simplify_list simp_func lst =
  let simplified_list = List.map simp_func lst in
  let extract
      (tup1 : 'a list * 'b )
      (tup2 : 'a list * 'b list)
    : 'a list * 'b list =
    (fst tup1 @ fst tup2, (snd tup1)::(snd tup2)) in
  let bindings, results =
    List.fold_right extract simplified_list ([], []) in
  bindings, results
;;
