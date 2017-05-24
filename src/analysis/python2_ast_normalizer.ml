module Abstract = Python2_abstract_ast
module Normalized = Python2_normalized_ast

let uid_of_annot _ = 0;;

let map_and_concat (func : 'a -> 'b list) (lst : 'a list) =
  List.concat (List.map func lst);;

let normalize_option func o =
  match o with
  | None -> None
  | Some(x) -> Some(func x)

let rec normalize_modl m : Normalized.modl =
  match m with
  | Abstract.Module (body, annot) ->
    Normalized.Module(map_and_concat normalize_stmt body, uid_of_annot annot)

and normalize_stmt s : Normalized.stmt list =
  match s with
  | Abstract.FunctionDef (_,_,_,_,_)-> [] (* TODO *)
  | Abstract.Return (value, annot) ->
    begin
      match value with
      | None -> [Normalized.Return(None, uid_of_annot annot)]
      | Some(x) ->
        let bindings, result = normalize_expr x in
        bindings @ [Normalized.Return(Some(result), uid_of_annot annot)]
    end
  | Abstract.Assign (targets, value, annot) ->
    let value_bindings, value_result = normalize_expr value in
    let target_bindings, target_results = normalize_expr_list targets in
    let bindings = value_bindings @ target_bindings in
    let assignments =
      List.map
        (fun e ->
           let e_uid = Normalized.uid_of_simple_expr e in
           Normalized.Assign(e, Normalized.SimpleExpr(value_result, e_uid), uid_of_annot annot))
        target_results in
    bindings @ assignments


  | _ -> []

(* Given an abstract expr, returns a list of statements, corresponding to
   the assignments necessary to compute it, and the name of the final
   variable that was bound *)
and normalize_expr
    (e : 'a Abstract.expr)
  : Normalized.stmt list * Normalized.simple_expr =
  match e with
  | Abstract.Num (n, annot) -> ([], Normalized.Num(normalize_number n, uid_of_annot annot))
  | _ -> [], Normalized.Str(0)

(* Given a list of exprs, returns a list containing all of their
   bindings and a list containing all of the relevant variable names *)
and normalize_expr_list
    (lst : 'a Abstract.expr list)
  : Normalized.stmt list * Normalized.simple_expr list =
  let normalized_list = List.map normalize_expr lst in
  let extract
      (tup1 : 'a list * 'b )
      (tup2 : 'a list * 'b list)
    : 'a list * 'b list =
    (fst tup1 @ fst tup2, (snd tup1)::(snd tup2)) in
  let bindings, results =
    List.fold_right extract normalized_list ([], []) in
  bindings, results

and normalize_slice s = (* May not be necessary *)
  match s with
  | _ -> []

and normalize_boolop b =
  match b with
  | Abstract.And -> Normalized.And
  | Abstract.Or -> Normalized.Or

and normalize_operator o =
  match o with
  | Abstract.Add -> Normalized.Add
  | Abstract.Sub -> Normalized.Sub
  | Abstract.Mult -> Normalized.Mult
  | Abstract.Div -> Normalized.Div
  | Abstract.Mod -> Normalized.Mod
  | Abstract.Pow -> Normalized.Pow

and normalize_uaryop o =
  match o with
  | Abstract.Not -> Normalized.Not
  | Abstract.UAdd -> Normalized.UAdd
  | Abstract.USub -> Normalized.USub

and normalize_cmpop o =
  match o with
  | Abstract.Eq -> Normalized.Eq
  | Abstract.NotEq -> Normalized.NotEq
  | Abstract.Lt -> Normalized.Lt
  | Abstract.LtE -> Normalized.LtE
  | Abstract.Gt -> Normalized.Gt
  | Abstract.GtE -> Normalized.GtE
  | Abstract.In -> Normalized.In
  | Abstract.NotIn -> Normalized.NotIn

and normalize_arguments a : Normalized.stmt list * Normalized.arguments =
  match a with
  | (args, varargs, kwargs, defaults) ->
    let arg_bindings, arg_results =
      normalize_expr_list args in
    let normalized_varargs =
      normalize_option (fun x -> x) varargs in
    let normalized_kwargs =
      normalize_option (fun x -> x) kwargs in
    let defaults_bindings, defaults_results =
      normalize_expr_list defaults in
    let bindings =
      arg_bindings @ defaults_bindings in
    bindings, (arg_results, normalized_varargs,
               normalized_kwargs, defaults_results)


and normalize_keyword k : Normalized.stmt list * Normalized.keyword =
  match k with
  | (id, value) ->
    let bindings, result = normalize_expr value in
    bindings, (id, result)

and normalize_sign s =
  match s with
  | Abstract.Pos -> Normalized.Pos
  | Abstract.Neg -> Normalized.Neg
  | Abstract.Zero -> Normalized.Zero

and normalize_number n =
  match n with
  | Abstract.Int sgn -> Normalized.Int(normalize_sign sgn)
  | Abstract.Float sgn -> Normalized.Float(normalize_sign sgn)
