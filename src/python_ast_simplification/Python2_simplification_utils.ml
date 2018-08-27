open Batteries;;
open Python2_ast_types;;
module Augmented = Python2_augmented_ast;;
module Simplified = Python2_simplified_ast;;
exception Invalid_assignment of string;;

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
    (result : identifier) (* The variable we assign the result/NotImplemented value to *)
    (obj : identifier) (* Variable we search for opFunc *)
    (arg : identifier) (* Name of the value which we pass to the opFunc *)
    (opFunc : string) (* Name of the builtin function we're looking for *)
  : Augmented.annotated_stmt m =
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

  return final_try
;;

let check_if_notimplemented (x0 : identifier) : Augmented.annotated_expr m =
  let%bind annot = get_annot () in
  let annotate = annotate annot in
  let is_notimplemented = annotate @@
    Augmented.Compare(annotate @@ Augmented.Name(x0),
                      [Augmented.Is],
                      [annotate @@ Augmented.Builtin(Builtin_NotImplemented)]
                     )
  in
  return is_notimplemented
;;

let check_if_same_type (x1 : identifier) (x2 : identifier)
  : Augmented.annotated_expr m =
  let%bind annot = get_annot () in
  let annotate = annotate annot in
  let is_same_type = annotate @@
    Augmented.Compare(
      annotate @@ Augmented.Call(
        annotate @@ Augmented.Builtin(Builtin_type),
        [annotate @@ Augmented.Name(x1)]),
      [Augmented.Is],
      [annotate @@ Augmented.Call(
          annotate @@ Augmented.Builtin(Builtin_type),
          [annotate @@ Augmented.Name(x2)])]
    )
  in
  return is_same_type
;;

let get_setup_and_final_assignment_for_augmented_assignment
    (target : Augmented.annotated_expr)
    (result : identifier)
    simplify_expr (* Needed for recursion *)
    simplify_slice (* Needed for recursion *)
  : (identifier * Augmented.annotated_stmt) m
  =
  (* We could do all of this in one enormous match statement, but I'm splitting
     it into two parts to make it easier to read *)

  (* Part 1: Make sure target has a valid form *)
  begin
    match target.body with
    | Augmented.Name _
    | Augmented.Attribute _
    | Augmented.Subscript _ -> () (* Valid *)
    (* All other targets are invalid. Raise an error *)
    | Augmented.List _
    | Augmented.Tuple _
      -> raise @@ Invalid_assignment "illegal expression for augmented assignment"
    | Augmented.BoolOp _
    | Augmented.BinOp _
    | Augmented.UnaryOp _
      -> raise @@ Invalid_assignment "can't assign to operator"
    | Augmented.IfExp _
      -> raise @@ Invalid_assignment "can't assign to conditional expression"
    | Augmented.Compare _
      -> raise @@ Invalid_assignment "can't assign to comparison"
    | Augmented.Call _
      -> raise @@ Invalid_assignment "can't assign to function call"
    | Augmented.Num _
    | Augmented.Str _
    | Augmented.Bool _
      -> raise @@ Invalid_assignment "can't assign to literal"
    | Augmented.NoneExpr
      -> raise @@ Invalid_assignment "cannot assign to None"
    | Augmented.Builtin _
      -> raise @@ Invalid_assignment "Can't assign to builtin. How did you even do that?"
  end
  ;


  let%bind annot = get_annot () in
  let annotate x = annotate annot x in
  let assign target =
    annotate @@
    Augmented.Assign([target],
                     annotate @@ Augmented.Name(result))
  in

  (* Part 2: Do any setup necessary, and figure out how to assign the value at
     the end *)
  match target.body with
  | Augmented.Name x ->
    return (x, assign @@ annotate @@ Augmented.Name(x))

  | Augmented.Attribute (obj, id) ->
    let%bind obj_result = simplify_expr obj in
    let%bind setup_result = simplify_expr @@ annotate @@
      Augmented.Attribute (annotate @@ Augmented.Name(obj_result), id)
    in
    return (setup_result,
            assign @@ annotate @@
            Augmented.Attribute (annotate @@ Augmented.Name(obj_result), id)
           )

  | Augmented.Subscript (lst, slice) ->
    let%bind lst_result = simplify_expr lst in
    let%bind slice_result = simplify_slice slice in
    let%bind setup_result = simplify_expr @@ annotate @@
      Augmented.Subscript(annotate @@ Augmented.Name(lst_result),
                          Augmented.Index(annotate @@ Augmented.Name(slice_result)))
    in
    return (setup_result,
            assign @@ annotate @@
            Augmented.Subscript(annotate @@ Augmented.Name(lst_result),
                                Augmented.Index(annotate @@ Augmented.Name(slice_result)))
           )

  | _ -> failwith "Bad target for augmented assignment" (* Other targets are invalid *)

;;
