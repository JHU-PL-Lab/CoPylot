open Batteries;;
open Python2_ast_types;;
module Augmented = Python2_augmented_ast;;
module Simplified = Python2_simplified_ast;;

open Python_simplification_monad;;
open Simplification_monad;;

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
  sequence @@ List.map simp_func lst
;;

let gen_assignment e =
  let%bind id = fresh_name () in
  let%bind _ = emit [Simplified.Assign(id, e)] in
  return id
;;

(* The binary operation translation uses two (three for augmented assignment)
   try statements which have similar forms -- see the "Binary Operation" part of
   the simplification spec for details *)
let gen_augmented_try_for_binop
    (simplify_expr) (* The simplify_expr func to allow for mutual recursion *)
    (result : identifier) (* The variable we assign the result/NotImplemented value to *)
    (obj : identifier) (* Variable we search for opFunc *)
    (opFunc : identifier) (* Name of the builtin function to do the binop *)
    (arg : identifier) (* Name of the other parameter which we pass to the opFunc *)
  : unit m =
  let%bind annot = get_annot () in
  let annotate body = annotate annot body in

  let%bind func_name = fresh_name () in

  (* x_3 = x_(e1).opFunc *)
  let opfunc_assignment = annotate @@ Augmented.Assign(
      [annotate @@ Augmented.Name(func_name)],
      annotate @@ Augmented.Attribute(annotate @@ Augmented.Name(obj), opFunc)
    )
  in

  let on_failure = Augmented.ExceptHandler(
      Some(annotate @@ Augmented.Builtin(Builtin_AttributeError)),
      None,
      [
        annotate @@Augmented.Assign(
          [annotate @@ Augmented.Name(result)],
          annotate @@ Augmented.Builtin(Builtin_NotImplemented))
      ]
    )
  in

  let on_success = annotate @@ Augmented.Assign(
      [annotate @@ Augmented.Name(result)],
      annotate @@ Augmented.Call(annotate @@ Augmented.Name(func_name),
                                 [annotate @@ Augmented.Name(arg)])
    )
  in

  let final_try = annotate @@
    Augmented.TryExcept(
      [opfunc_assignment],
      [on_failure],
      [on_success]
    )
  in

  simplify_expr final_try
;;
