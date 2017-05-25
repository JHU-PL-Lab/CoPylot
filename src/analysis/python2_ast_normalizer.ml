module Abstract = Python2_abstract_ast
module Normalized = Python2_normalized_ast

let uid_of_annot _ = 0;;

let gen_unique_name _ = "";;

let map_and_concat (func : 'a -> 'b list) (lst : 'a list) =
  List.concat (List.map func lst);;

let normalize_option func o =
  match o with
  | None -> None
  | Some(x) -> Some(func x)

(* Given a uid and a compound_expr, assigns that expr to a new, unique name.
   Returns the assignment statement (in a list) and the name used *)
let gen_normalized_assignment u e =
  (* TODO: Should we be generating more uids? *)
  let name = Normalized.Name(gen_unique_name u, u) in
  let assignment = Normalized.Assign(name, e, u) in
  [assignment], name

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
           Normalized.Assign(e,
                             Normalized.SimpleExpr(value_result, e_uid),
                             uid_of_annot annot)
        )
        target_results in
    bindings @ assignments
  | Abstract.AugAssign (target, op, value, annot) ->
    (* Convert to a standard assignment, then normalize that *)
    let regAssign =
      Abstract.Assign(
        [target],
        Abstract.BinOp(
          target,
          op,
          value,
          annot
        ),
        annot) in
    normalize_stmt regAssign
  | Abstract.Print (dest, values, nl, annot) ->
    let dest_bindings, dest_result = normalize_expr_option dest in
    let value_bindings, value_results = normalize_expr_list values in
    let u = uid_of_annot annot in
    let bindings = dest_bindings @ value_bindings in
    bindings @ [Normalized.Print(dest_result, value_results, nl, u)]
  | Abstract.For (_,_,_,_,_) -> [] (* TODO *)
  | Abstract.While (_,_,_,_) -> [] (* TODO *)
  | Abstract.If (_,_,_,_) -> [] (* TODO *)
  | Abstract.Expr (e, annot) ->
    let bindings, result = normalize_expr e in
    bindings @ [Normalized.SimpleExprStmt(result, uid_of_annot annot)]
  | Abstract.Pass (annot) ->
    [Normalized.Pass (uid_of_annot annot)]
  | Abstract.Break (_) -> [] (* TODO *)
  | Abstract.Continue (_) -> [] (* TODO *)

(* Given an abstract expr, returns a list of statements, corresponding to
   the assignments necessary to compute it, and the name of the final
   variable that was bound *)
and normalize_expr
    (e : 'a Abstract.expr)
  : Normalized.stmt list * Normalized.simple_expr =
  match e with
  | Abstract.BoolOp (op, values, annot) ->
    let value_bindings, value_results = normalize_expr_list values in
    let norm_op = normalize_boolop op in
    let u = uid_of_annot annot in
    (* Now value_results is a list of simple exprs, which we're operating on.
       Iterate through that list, replacing each pair with an assignment
       to a new value, which we then use as the first element of the next
       pair, etc. *)
    let combine
        (prev : Normalized.stmt list * Normalized.simple_expr)
        (next : Normalized.simple_expr)
      : Normalized.stmt list * Normalized.simple_expr =
      let assignment, name =
        gen_normalized_assignment
          u
          (Normalized.BoolOp(snd prev, norm_op, next, u)) in
      (fst prev) @ assignment, name
    in
    List.fold_left combine
      (* TODO: Throw a useful error if value_results is empty. Not sure if
         that's possible given how we generate these trees, but best to be safe. *)
      (value_bindings, (List.hd value_results))
      (List.tl value_results)

  | Abstract.BinOp (left, op, right, annot) ->
    let left_bindings, left_result = normalize_expr left in
    let right_bindings, right_result = normalize_expr right in
    let u = uid_of_annot annot in
    let assignment, name =
      gen_normalized_assignment
        u
        (Normalized.BinOp(left_result,
                          normalize_operator op,
                          right_result,
                          u)) in
    let bindings = left_bindings @ right_bindings @ assignment in
    bindings, name
  | Abstract.UnaryOp (op, operand, annot) ->
    let bindings, result = normalize_expr operand in
    let u = uid_of_annot annot in
    let assignment, name = gen_normalized_assignment u
        (Normalized.UnaryOp(normalize_unaryop op, result, u)) in
    bindings @ assignment, name
  | Abstract.Call (_,_,_,_,_,_) -> [], Normalized.Name("TODO", 0) (* TODO *)
  | Abstract.Num (n, annot) ->
    ([], Normalized.Num(normalize_number n, uid_of_annot annot))
  | Abstract.Str (annot) ->
    ([], Normalized.Str(uid_of_annot annot))
  | Abstract.Bool (b, annot) ->
    ([], Normalized.Bool(b, uid_of_annot annot))
  | Abstract.Subscript (value, slice, _, annot) ->
    let value_bindings, value_result = normalize_expr value in
    let slice_bindings, slice_result = normalize_slice slice in
    let u = uid_of_annot annot in
    let assignment, name =
      gen_normalized_assignment u (Normalized.Subscript(value_result,
                                                        slice_result,
                                                        u)) in
    let bindings = value_bindings @ slice_bindings @ assignment in
    bindings, name

  | Abstract.Name (id, _, annot) -> (* Throw out context *)
    ([], Normalized.Name(id, uid_of_annot annot))
  | Abstract.List (elts, _, annot) ->
    let bindings, results = normalize_expr_list elts in
    let u = uid_of_annot annot in
    let assignment, name =
      gen_normalized_assignment u (Normalized.List(results, u)) in
    bindings @ assignment, name
  | Abstract.Tuple (elts, _, annot) ->
    let bindings, results = normalize_expr_list elts in
    let u = uid_of_annot annot in
    let assignment, name =
      gen_normalized_assignment u (Normalized.Tuple(results, u)) in
    bindings @ assignment, name
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

and normalize_expr_option
    (o : 'a Abstract.expr option)
  : Normalized.stmt list * Normalized.simple_expr option =
  let normalized_opt = normalize_option normalize_expr o in
  match normalized_opt with
  | None -> [], None
  | Some(bindings, result) -> bindings, Some(result)


and normalize_slice (* May not be necessary *)
    (s : 'a Abstract.slice)
  : Normalized.stmt list * Normalized.slice =
  match s with
  | Abstract.Slice (lower, upper, step) ->
    let lower_bindings, lower_result = normalize_expr_option lower in
    let upper_bindings, upper_result = normalize_expr_option upper in
    let step_bindings, step_result = normalize_expr_option step in
    let bindings = lower_bindings @ upper_bindings @ step_bindings in
    bindings, Normalized.Slice(lower_result, upper_result, step_result)
  | Abstract.Index (value) ->
    let bindings, result = normalize_expr value in
    bindings, Normalized.Index(result)

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

and normalize_unaryop o =
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
