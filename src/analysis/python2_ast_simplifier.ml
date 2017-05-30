module Abstract = Python2_abstract_ast
module Simplified = Python2_simplified_ast

let gen_unique_name _ = "";;

let map_and_concat (func : 'a -> 'b list) (lst : 'a list) =
  List.concat (List.map func lst)
;;

let simplify_option func o =
  match o with
  | None -> None
  | Some(x) -> Some(func x)
;;

let rec simplify_modl m : 'a Simplified.modl =
  match m with
  | Abstract.Module (body, annot) ->
    Simplified.Module(map_and_concat simplify_stmt body, annot)

and simplify_stmt
    (s : 'a Abstract.stmt)
  : 'a Simplified.stmt list =
  match s with
  | Abstract.FunctionDef (func_name, args, body, _, annot)->
    let simplified_args = simplify_arguments args in
    let simplified_body = map_and_concat simplify_stmt body in
    [Simplified.FunctionDef(func_name,
                            simplified_args,
                            simplified_body,
                            annot)]

  | Abstract.Return (value, annot) ->
    [Simplified.Return(simplify_expr_option value, annot)]

  | Abstract.Assign _ -> [] (* TODO *)

  | Abstract.AugAssign (target, op, value, annot) ->
    (* Convert to a standard assignment, then simplify that *)
    let regAssign =
      Abstract.Assign(
        [target],
        Abstract.BinOp(target, op, value, annot),
        annot
      ) in
    simplify_stmt regAssign

  | Abstract.Print (dest, values, nl, annot) ->
    [Simplified.Print (
        simplify_expr_option dest,
        List.map simplify_expr values,
        nl,
        annot
      )]

  | Abstract.For (target, seq, body, _, annot) ->
    (* For loops are always over some iterable. According to the docs,
       they loop until they are told to stop, or their iterator stops
       returning objects.

       The targets are assigned to at the beginning of the body, then the
       body's code is executed. When there are no elements of the iterable
       to assign, the loop terminates.

       We handle for loops by turning them into an abstract while loop,
       then simplifying the resulting code. Specifically, we turn

       for i in seq:
         <body>

       into

       next_val = seq.__iter__().next
       try:
         while True:
           i = next_val()
           <body>
       except StopIteration:
         pass

    *)
    let next_val = gen_unique_name annot in
    let bind_next_val = (* next_val = seq.__iter__().next_val *)
      Abstract.Assign(
        [Abstract.Name(next_val, Abstract.Store, annot)],
        Abstract.Attribute(
          Abstract.Call(
            Abstract.Attribute(
              seq,
              "__iter__",
              Abstract.Load,
              annot
            ),
            [],
            [],
            None,
            None,
            annot
          ),
          "next",
          Abstract.Load,
          annot
        ),
        annot
      ) in
    let assign_target = (* i = next_val() *)
      Abstract.Assign(
        [target],
        Abstract.Call(
          Abstract.Name(next_val, Abstract.Load, annot),
          [],
          [],
          None,
          None,
          annot
        ),
        annot
      ) in
    let while_loop = (* while True: i = next_val(); <body> *)
      Abstract.While(
        Abstract.Name("True", Abstract.Load, annot),
        [assign_target] @ body,
        [],
        annot
      ) in
    let try_except = (* try: <while>; except StopIteration: pass *)
      Abstract.TryExcept(
        [while_loop],
        [Abstract.ExceptHandler(
            Some(Abstract.Name("StopIteration", Abstract.Load, annot)),
            None,
            [Abstract.Pass(annot)],
            annot
          )],
        [],
        annot
      ) in
    map_and_concat simplify_stmt [bind_next_val; try_except]
  | Abstract.While (test, body, _, annot) ->
    [Simplified.While(simplify_expr test,
                      map_and_concat simplify_stmt body,
                      annot)]

  | Abstract.If (test, body, orelse, annot) ->
    [Simplified.If(simplify_expr test,
                   map_and_concat simplify_stmt body,
                   map_and_concat simplify_stmt orelse,
                   annot)]

  | Abstract.Raise (typ, value, _, annot) ->
    [Simplified.Raise(
        simplify_expr_option typ,
        simplify_expr_option value,
        annot)]

  | Abstract.TryExcept _ -> [] (* TODO *)

  | Abstract.Expr (e, annot) ->
    [Simplified.Expr(simplify_expr e, annot)]

  | Abstract.Pass (annot) ->
    [Simplified.Pass (annot)]

  | Abstract.Break (annot) ->
    [Simplified.Break (annot)]

  | Abstract.Continue (annot) ->
    [Simplified.Continue(annot)]

(* Given an abstract expr, returns a list of statements, corresponding to
   the assignments necessary to compute it, and the name of the final
   variable that was bound *)
and simplify_expr
    (e : 'a Abstract.expr)
  : 'a Simplified.expr =
  match e with
  | Abstract.BoolOp (op, values, annot) ->
    Simplified.BoolOp (simplify_boolop op,
                       List.map simplify_expr values,
                       annot)

  | Abstract.BinOp (left, op, right, annot) ->
    Simplified.BinOp (simplify_expr left,
                      simplify_operator op,
                      simplify_expr right,
                      annot)

  | Abstract.UnaryOp (op, operand, annot) ->
    Simplified.UnaryOp (simplify_unaryop op,
                        simplify_expr operand,
                        annot)

  | Abstract.IfExp (test, body, orelse, annot) ->
    Simplified.IfExp (simplify_expr test,
                      simplify_expr body,
                      simplify_expr orelse,
                      annot)

  | Abstract.Compare (left, ops, comparators, annot) ->
    Simplified.Compare (simplify_expr left,
                        List.map simplify_cmpop ops,
                        List.map simplify_expr comparators,
                        annot)

  | Abstract.Call (func, args, _, _, _, annot) ->
    Simplified.Call (simplify_expr func,
                     List.map simplify_expr args,
                     annot)

  | Abstract.Num (n, annot) ->
    Simplified.Num(simplify_number n, annot)

  | Abstract.Str (annot) ->
    Simplified.Str(annot)

  | Abstract.Bool (b, annot) ->
    Simplified.Bool(b, annot)

  | Abstract.Attribute (obj, attr, _, annot) ->
    Simplified.Attribute (simplify_expr obj, attr, annot)

  (* Turn subscripts into calls to __getitem__() *)
  | Abstract.Subscript (value, slice, _, annot) ->
    Simplified.Call(simplify_expr value,
                    [simplify_slice slice annot],
                    annot)

  | Abstract.Name (id, _, annot) -> (* Throw out context *)
    Simplified.Name(id, annot)

  | Abstract.List (elts, _, annot) ->
    Simplified.List (List.map simplify_expr elts, annot)

  | Abstract.Tuple (elts, _, annot) ->
    Simplified.Tuple (List.map simplify_expr elts, annot)

and simplify_expr_option
    (o : 'a Abstract.expr option)
  : 'a Simplified.expr option =
  let simplified_opt = simplify_option simplify_expr o in
  match simplified_opt with
  | None -> None
  | Some(result) -> Some(result)

(* Turn a slice operator into a call to the slice() function, with
   the same arguments. E.g. 1:2:3 becomes slice(1,2,3), and
   1:2 becomes slice (1,2,None) *)
and simplify_slice
    (s : 'a Abstract.slice)
    annot
  : 'a Simplified.expr =
  (* Turn a "None" option into the python "None" object, and turn a
     "Some" option into the simplified version of its contents *)
  let exp_opt_to_slice_arg e =
    match e with
    | None ->
      Simplified.Name("None", annot)
    | Some(x) ->
      simplify_expr x
  in
  let args_list =
    begin
      match s with
      | Abstract.Slice (lower, upper, step) ->
        [
          exp_opt_to_slice_arg lower;
          exp_opt_to_slice_arg upper;
          exp_opt_to_slice_arg step;
        ]
      | Abstract.Index (value) -> [simplify_expr value]
    end
  in
  Simplified.Call(
    Simplified.Name("slice", annot),
    args_list,
    annot
  )

and simplify_boolop b =
  match b with
  | Abstract.And -> Simplified.And
  | Abstract.Or -> Simplified.Or

and simplify_operator o =
  match o with
  | Abstract.Add -> Simplified.Add
  | Abstract.Sub -> Simplified.Sub
  | Abstract.Mult -> Simplified.Mult
  | Abstract.Div -> Simplified.Div
  | Abstract.Mod -> Simplified.Mod
  | Abstract.Pow -> Simplified.Pow

and simplify_unaryop o =
  match o with
  | Abstract.Not -> Simplified.Not
  | Abstract.UAdd -> Simplified.UAdd
  | Abstract.USub -> Simplified.USub

and simplify_cmpop o =
  match o with
  | Abstract.Eq -> Simplified.Eq
  | Abstract.NotEq -> Simplified.NotEq
  | Abstract.Lt -> Simplified.Lt
  | Abstract.LtE -> Simplified.LtE
  | Abstract.Gt -> Simplified.Gt
  | Abstract.GtE -> Simplified.GtE
  | Abstract.In -> Simplified.In
  | Abstract.NotIn -> Simplified.NotIn

and simplify_arguments a : 'a Simplified.expr list =
  match a with
  | (args, _, _, _) ->
    List.map simplify_expr args

and simplify_sign s =
  match s with
  | Abstract.Pos -> Simplified.Pos
  | Abstract.Neg -> Simplified.Neg
  | Abstract.Zero -> Simplified.Zero

and simplify_number n =
  match n with
  | Abstract.Int sgn -> Simplified.Int(simplify_sign sgn)
  | Abstract.Float sgn -> Simplified.Float(simplify_sign sgn)
