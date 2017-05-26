module Abstract = Python2_abstract_ast
module Simplified = Python2_simplified_ast

let get_next_uid _ = 0;;

let gen_unique_name _ = "";;

let map_and_concat (func : 'a -> 'b list) (lst : 'a list) =
  List.concat (List.map func lst)
;;

let simplify_option func o =
  match o with
  | None -> None
  | Some(x) -> Some(func x)
;;

(* Given a uid and a compound_expr, assigns that expr to a new, unique name.
   Returns the assignment statement (in a list) and the name used *)
let gen_simplified_assignment annot e =
  let u = get_next_uid annot in
  let name = Simplified.Name(gen_unique_name u, u) in
  let assignment = Simplified.Assign(name, e, get_next_uid annot) in
  [assignment], name
;;

let rec simplify_modl m : Simplified.modl =
  match m with
  | Abstract.Module (body, annot) ->
    Simplified.Module(map_and_concat simplify_stmt body, get_next_uid annot)

(* We need some additional arguments when we're inside a loop,
   so that Break and Continue know what to do. These are only neded in that
   special case, though, so it's convenient to hide them in other cases. *)
and simplify_stmt s = simplify_stmt_full None None s

and simplify_stmt_full
    loop_start_uid
    loop_end_uid
    (s : 'a Abstract.stmt)
  : Simplified.stmt list =
  match s with
  | Abstract.FunctionDef (func_name, args, body, _, annot)->
    (* Hopefully the args are just names so this won't do anything *)
    let arg_bindings, simplified_args = simplify_arguments args in
    let simplified_body = map_and_concat simplify_stmt body in
    arg_bindings @
    [Simplified.FunctionDef(func_name,
                            simplified_args,
                            simplified_body,
                            get_next_uid annot)]

  | Abstract.Return (value, annot) ->
    begin
      match value with
      | None -> [Simplified.Return(None, get_next_uid annot)]
      | Some(x) ->
        let bindings, result = simplify_expr x in
        bindings @ [Simplified.Return(Some(result), get_next_uid annot)]
    end

  (* TODO: What if we're assigning from a tuple? *)
  | Abstract.Assign (targets, value, annot) ->
    let value_bindings, value_result = simplify_expr value in
    (* TODO: Go through and unpack tuples in an appropriate way, I guess *)
    let target_bindings, target_results = List.map simplify_expr targets in
    let bindings = value_bindings @ target_bindings in
    let assignments =
      List.map
        (fun e ->
           let e_uid = Simplified.uid_of_simple_expr e in
           Simplified.Assign(e,
                             Simplified.SimpleExpr(value_result, e_uid),
                             get_next_uid annot)
        )
        target_results in
    bindings @ assignments

  | Abstract.AugAssign (target, op, value, annot) ->
    (* Convert to a standard assignment, then simplify that *)
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
    simplify_stmt regAssign

  | Abstract.Print (dest, values, nl, annot) ->
    let dest_bindings, dest_result = simplify_expr_option dest in
    let value_bindings, value_results = List.map simplify_expr values in
    let bindings = dest_bindings @ value_bindings in
    bindings @ [Simplified.Print(dest_result,
                                 value_results,
                                 nl,
                                 get_next_uid annot)]

  | Abstract.For (target, seq, body, _, annot) ->
    (* For loops are always over some iterable. According to the docs,
       they loop until they are told to stop, or their iterator stops
       returning objects.

       The targets are assigned to at the beginning of the body, then the
       body's code is executed. When there are no elements of the iterable
       to assign, the loop terminates.
       TODO: So far this assume we're only assigning to one target *)
    let target_bindings, target_name = simplify_expr target in
    (* e.g. Assign the sequence to a value seq_name *)
    let seq_bindings, seq_name = simplify_expr seq in
    (* We want to end up with a variable holding the next() method of
       an iterable for seq. This involves a number of different expression
       types, so we build the corresponding abstract expression and then
       simplify that. Specifically, we build
       <name> = seq_name.__iter__().next *)
    let next_name = gen_unique_name annot in
    let next_abstract_stmt =
      Abstract.Assign(
        [Abstract.Name(next_name, Abstract.Store, annot)],
        Abstract.Attribute(
          Abstract.Call(
            Abstract.Attribute(
              Abstract.Name(
                begin
                  match seq_name with
                  | Simplified.Name (s,_) -> s
                  | _ -> "BAD" (* TODO: Should be impossible *)
                end,
                Abstract.Load,
                annot),
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
    let next_bindings = simplify_stmt next_abstract_stmt in
    (* So now we have the next() function bound to a variable called
       next_name. The start of the loop will be a call to this function. *)
    let next_call =
      Simplified.Call(Simplified.Name(next_name, get_next_uid annot),
                      [], get_next_uid annot) in
    let start_uid = get_next_uid annot in (* Start label *)
    let loop_start =
      Simplified.Assign(target_name, next_call, start_uid) in
    let end_uid = get_next_uid annot in (* End label *)
    let simplified_body =
      (map_and_concat
         (simplify_stmt_full
            (Some(start_uid))
            (Some(end_uid)))
         body) in
    let loop_end = Simplified.Pass(end_uid) in
    target_bindings @ seq_bindings @ next_bindings @ [loop_start] @
    simplified_body @ [loop_end]
  | Abstract.While (test, body, _, annot) ->
    let test_bindings, test_name =
      (* It's nicer to have "if !test then goto end" as opposed to
         "if test then pass else goto end" *)
      simplify_expr (Abstract.UnaryOp(Abstract.Not,
                                      test,
                                      get_next_uid annot)) in

    let start_uid = get_next_uid annot in (* Start label *)
    let end_uid = get_next_uid annot in (* End label *)
    let test_at_beginning =
      Simplified.If(test_name,
                    [Simplified.Goto(end_uid, get_next_uid annot)],
                    [],
                    start_uid) in
    let simplified_body =
      (map_and_concat
         (simplify_stmt_full
            (Some(start_uid))
            (Some(end_uid)))
         body) in
    let end_stmt = Simplified.Pass(end_uid) in
    test_bindings @ [test_at_beginning] @ simplified_body @ [end_stmt]

  | Abstract.If (test, body, orelse, annot) ->
    let test_bindings, test_name = simplify_expr test in
    let simplified_body = map_and_concat simplify_stmt body in
    let simplified_orelse = map_and_concat simplify_stmt orelse in
    test_bindings @
    [Simplified.If(test_name,
                   simplified_body,
                   simplified_orelse,
                   get_next_uid annot)]
  | Abstract.Raise (typ, value, _, annot) ->
    let type_binding, type_result = simplify_expr_option typ in
    let value_binding, value_result = simplify_expr_option value in
    type_binding @ value_binding @
    [Simplified.Raise(type_result, value_result, get_next_uid annot)]
  | Abstract.TryExcept _ -> [] (* TODO *)
  | Abstract.Expr (e, annot) ->
    let bindings, result = simplify_expr e in
    bindings @ [Simplified.SimpleExprStmt(result, get_next_uid annot)]

  | Abstract.Pass (annot) ->
    [Simplified.Pass (get_next_uid annot)]

  | Abstract.Break (annot) ->
    begin
      match loop_end_uid with
      | None -> [] (* TODO: Throw error for break outside loop *)
      | Some(u) -> [Simplified.Goto(u, get_next_uid annot)]
    end

  | Abstract.Continue (annot) ->
    begin
      match loop_start_uid with
      | None -> [] (* TODO: Throw error for continue outside loop *)
      | Some(u) -> [Simplified.Goto(u, get_next_uid annot)]
    end

(* Given an abstract expr, returns a list of statements, corresponding to
   the assignments necessary to compute it, and the name of the final
   variable that was bound *)
and simplify_expr
    (e : 'a Abstract.expr)
  : Simplified.compound_expr =
  match e with
  | Abstract.BoolOp (op, values, annot) ->
    Simplified.BoolOp (simplify_boolop op,
                       List.map simplify_expr values,
                       get_next_uid annot)

  | Abstract.BinOp (left, op, right, annot) ->
    Simplified.BinOp (simplify_expr left,
                      simplify_operator op,
                      simplify_expr right,
                      get_next_uid annot)

  | Abstract.UnaryOp (op, operand, annot) ->
    Simplified.UnaryOp (simplify_unaryop op,
                        simplify_expr operand,
                        get_next_uid annot)

  | Abstract.IfExp (test, body, orelse, annot) ->
    Simplified.IfExp (simplify_expr test,
                      simplify_expr body,
                      simplify_expr orelse,
                      get_next_uid annot)

  | Abstract.Compare (left, ops, comparators, annot) ->
    Simplified.Compare (simplify_expr left,
                        List.map simplify_cmpop ops,
                        List.map simplify_expr comparators,
                        get_next_uid annot)

  | Abstract.Call (func, args, _, _, _, annot) ->
    Simplified.Call (simplify_expr func,
                     List.map simplify_expr args,
                     get_next_uid annot)

  | Abstract.Num (n, annot) ->
    Simplified.SimpleExpr(
      Simplified.Num(simplify_number n, get_next_uid annot),
      get_next_uid annot)

  | Abstract.Str (annot) ->
    Simplified.SimpleExpr(Simplified.Str(get_next_uid annot),
                          get_next_uid annot)

  | Abstract.Bool (b, annot) ->
    Simplified.SimpleExpr(Simplified.Bool(b, get_next_uid annot),
                          get_next_uid annot)

  | Abstract.Attribute (obj, attr, _, annot) ->
    Simplified.Attribute (simplify_expr obj, attr, get_next_uid annot)

  (* Turn subscripts into calls to __getitem__() *)
  | Abstract.Subscript (value, slice, _, annot) ->
    Simplified.Call(simplify_expr value,
                    [simplify_slice slice annot],
                    get_next_uid annot)

  | Abstract.Name (id, _, annot) -> (* Throw out context *)
    Simplified.SimpleExpr(Simplified.Name(id, get_next_uid annot),
                          get_next_uid annot)

  | Abstract.List (elts, _, annot) ->
    Simplified.List (List.map simplify_expr elts, get_next_uid annot)

  | Abstract.Tuple (elts, _, annot) ->
    Simplified.Tuple (List.map simplify_expr elts, get_next_uid annot)

and simplify_expr_option
    (o : 'a Abstract.expr option)
  : Simplified.compound_expr option =
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
  : Simplified.compound_expr =
  (* Turn a "None" option into the python "None" object, and turn a
     "Some" option into the simplified version of its contents *)
  let exp_opt_to_slice_arg e =
    match e with
    | None ->
      Simplified.SimpleExpr(
        Simplified.Name("None", get_next_uid annot),
        get_next_uid annot)
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
    Simplified.SimpleExpr(Simplified.Name("slice", get_next_uid annot),
                          get_next_uid annot),
    args_list,
    get_next_uid annot
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

and simplify_arguments a : Simplified.compound_expr list =
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
