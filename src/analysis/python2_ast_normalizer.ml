module Abstract = Python2_abstract_ast
module Normalized = Python2_normalized_ast

let get_next_uid _ = 0;;

let gen_unique_name _ = "";;

let map_and_concat (func : 'a -> 'b list) (lst : 'a list) =
  List.concat (List.map func lst)
;;

let normalize_option func o =
  match o with
  | None -> None
  | Some(x) -> Some(func x)
;;

(* Given a uid and a compound_expr, assigns that expr to a new, unique name.
   Returns the assignment statement (in a list) and the name used *)
let gen_normalized_assignment annot e =
  let u = get_next_uid annot in
  let name = Normalized.Name(gen_unique_name u, u) in
  let assignment = Normalized.Assign(name, e, get_next_uid annot) in
  [assignment], name
;;

(* Most normalize fuctions return a list of statements and the name of the
   variable the last statement bound to. This means that apply List.map to them
   gives a list of tuples; this extracts them into two separate lists. *)
let normalize_list normalize_func lst =
  let normalized_list = List.map normalize_func lst in
  let extract
      (tup1 : 'a list * 'b )
      (tup2 : 'a list * 'b list)
    : 'a list * 'b list =
    (fst tup1 @ fst tup2, (snd tup1)::(snd tup2)) in
  let bindings, results =
    List.fold_right extract normalized_list ([], []) in
  bindings, results
;;

let rec normalize_modl m : Normalized.modl =
  match m with
  | Abstract.Module (body, annot) ->
    Normalized.Module(map_and_concat normalize_stmt body, get_next_uid annot)

(* We need some additional arguments when we're inside a loop,
   so that Break and Continue know what to do. These are only neded in that
   special case, though, so it's convenient to hide them in other cases. *)
and normalize_stmt s = normalize_stmt_full None None s

and normalize_stmt_full
    loop_start_uid
    loop_end_uid
    (s : 'a Abstract.stmt)
  : Normalized.stmt list =
  match s with
  | Abstract.FunctionDef (func_name, args, body, _, annot)->
    (* Hopefully the args are just names so this won't do anything *)
    let arg_bindings, normalized_args = normalize_arguments args in
    let normalized_body = map_and_concat normalize_stmt body in
    arg_bindings @
    [Normalized.FunctionDef(func_name,
                            normalized_args,
                            normalized_body,
                            get_next_uid annot)]

  | Abstract.Return (value, annot) ->
    begin
      match value with
      | None -> [Normalized.Return(None, get_next_uid annot)]
      | Some(x) ->
        let bindings, result = normalize_expr x in
        bindings @ [Normalized.Return(Some(result), get_next_uid annot)]
    end

  (* TODO: What if we're assigning from a tuple? *)
  | Abstract.Assign (targets, value, annot) ->
    let value_bindings, value_result = normalize_expr value in
    (* TODO: Go through and unpack tuples in an appropriate way, I guess *)
    let target_bindings, target_results = normalize_expr_list targets in
    let bindings = value_bindings @ target_bindings in
    let assignments =
      List.map
        (fun e ->
           let e_uid = Normalized.uid_of_simple_expr e in
           Normalized.Assign(e,
                             Normalized.SimpleExpr(value_result, e_uid),
                             get_next_uid annot)
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
    let bindings = dest_bindings @ value_bindings in
    bindings @ [Normalized.Print(dest_result,
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
    let target_bindings, target_name = normalize_expr target in
    (* e.g. Assign the sequence to a value seq_name *)
    let seq_bindings, seq_name = normalize_expr seq in
    (* We want to end up with a variable holding the next() method of
       an iterable for seq. This involves a number of different expression
       types, so we build the corresponding abstract expression and then
       normalize that. Specifically, we build
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
                  | Normalized.Name (s,_) -> s
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
    let next_bindings = normalize_stmt next_abstract_stmt in
    (* So now we have the next() function bound to a variable called
       next_name. The start of the loop will be a call to this function. *)
    let next_call =
      Normalized.Call(Normalized.Name(next_name, get_next_uid annot),
                      [], get_next_uid annot) in
    let start_uid = get_next_uid annot in (* Start label *)
    let loop_start =
      Normalized.Assign(target_name, next_call, start_uid) in
    let end_uid = get_next_uid annot in (* End label *)
    let normalized_body =
      (map_and_concat
         (normalize_stmt_full
            (Some(start_uid))
            (Some(end_uid)))
         body) in
    let loop_end = Normalized.Pass(end_uid) in
    target_bindings @ seq_bindings @ next_bindings @ [loop_start] @
    normalized_body @ [loop_end]
  | Abstract.While (test, body, _, annot) ->
    let test_bindings, test_name =
      (* It's nicer to have "if !test then goto end" as opposed to
         "if test then pass else goto end" *)
      normalize_expr (Abstract.UnaryOp(Abstract.Not,
                                       test,
                                       get_next_uid annot)) in

    let start_uid = get_next_uid annot in (* Start label *)
    let end_uid = get_next_uid annot in (* End label *)
    let test_at_beginning =
      Normalized.If(test_name,
                    [Normalized.Goto(end_uid, get_next_uid annot)],
                    [],
                    start_uid) in
    let normalized_body =
      (map_and_concat
         (normalize_stmt_full
            (Some(start_uid))
            (Some(end_uid)))
         body) in
    let end_stmt = Normalized.Pass(end_uid) in
    test_bindings @ [test_at_beginning] @ normalized_body @ [end_stmt]

  | Abstract.If (test, body, orelse, annot) ->
    let test_bindings, test_name = normalize_expr test in
    let normalized_body = map_and_concat normalize_stmt body in
    let normalized_orelse = map_and_concat normalize_stmt orelse in
    test_bindings @
    [Normalized.If(test_name,
                   normalized_body,
                   normalized_orelse,
                   get_next_uid annot)]

  | Abstract.Expr (e, annot) ->
    let bindings, result = normalize_expr e in
    bindings @ [Normalized.SimpleExprStmt(result, get_next_uid annot)]

  | Abstract.Pass (annot) ->
    [Normalized.Pass (get_next_uid annot)]

  | Abstract.Break (annot) ->
    begin
      match loop_end_uid with
      | None -> [] (* TODO: Throw error for break outside loop *)
      | Some(u) -> [Normalized.Goto(u, get_next_uid annot)]
    end

  | Abstract.Continue (annot) ->
    begin
      match loop_start_uid with
      | None -> [] (* TODO: Throw error for continue outside loop *)
      | Some(u) -> [Normalized.Goto(u, get_next_uid annot)]
    end

(* Given an abstract expr, returns a list of statements, corresponding to
   the assignments necessary to compute it, and the name of the final
   variable that was bound *)
and normalize_expr
    (e : 'a Abstract.expr)
  : Normalized.stmt list * Normalized.simple_expr =
  match e with
  (* BoolOps are a tricky case because of short-circuiting. We need to
     make sure that when we evaluate "a and False and b", b is never
     evaluated, etc.

     We do this by iteratively breaking down the statements like so:
     "a and b and c and ..." turns into
     "test1 = a
      if test1:
        test2 = b
     else
       test2 = False
     test3 = test1 and test2
     test4 = test3 and c and ..."
     We then apply the same process to break down the assignment to test4
     and continue in this manner until we hit the end of the statement. *)
  | Abstract.BoolOp (op, values, annot) ->
    (* TODO: Throw a useful error if values is empty. Not sure if that's
       possible given how we generate these trees, but best to be safe. *)
    let first_arg = List.hd values in
    let remaining_args = List.tl values in
    let norm_op = normalize_boolop op in
    (* Performs the single step of decomposition described above *)
    let combine
        (prev : Normalized.stmt list * Normalized.simple_expr)
        (next : 'a Abstract.expr)
      : Normalized.stmt list * Normalized.simple_expr =
      let bindings, name = normalize_expr next in
      let body, orelse =
        match op with
        | Abstract.And ->
          bindings,
          [Normalized.Assign(
              name,
              Normalized.SimpleExpr(Normalized.Bool(false, get_next_uid annot),
                                    get_next_uid annot),
              get_next_uid annot)]
        | Abstract.Or ->
          [Normalized.Assign(
              name,
              Normalized.SimpleExpr(Normalized.Bool(true, get_next_uid annot),
                                    get_next_uid annot),
              get_next_uid annot)],
          bindings
      in
      let big_if = Normalized.If(snd prev, body, orelse, get_next_uid annot) in
      let assignment, name2 =
        gen_normalized_assignment annot
          (Normalized.BoolOp(snd prev, norm_op, name, get_next_uid annot)) in
      let bindings = (fst prev) @ [big_if] @ assignment in
      bindings, name2
    in (* End definition of combine *)
    let first_arg_bindings, first_arg_result = normalize_expr first_arg in
    List.fold_left combine
      (first_arg_bindings, first_arg_result)
      remaining_args

  | Abstract.BinOp (left, op, right, annot) ->
    let left_bindings, left_result = normalize_expr left in
    let right_bindings, right_result = normalize_expr right in
    let assignment, name =
      gen_normalized_assignment
        annot
        (Normalized.BinOp(left_result,
                          normalize_operator op,
                          right_result,
                          get_next_uid annot)) in
    let bindings = left_bindings @ right_bindings @ assignment in
    bindings, name
  | Abstract.UnaryOp (op, operand, annot) ->
    let bindings, result = normalize_expr operand in
    let assignment, name = gen_normalized_assignment annot
        (Normalized.UnaryOp(normalize_unaryop op, result, get_next_uid annot)) in
    bindings @ assignment, name
  | Abstract.IfExp (test, body, orelse, annot) ->
    (* Python allows expressions like x = 1 if test else 0. Of course,
       only the relevant branch is executed, so we can't simply evaluate both
       beforehand. But in order to be on the right-hand-side of an assignment,
       the expression must be no more complicated than a compound_expr. In
       particular, the expression can't be an assignment.

       So to evaluate x = y if test else z, we first create an if _statement_
       and then use the results of that.
       if test:
         # evaluate and bind test1 = y
       else:
         # evaluate and bind test2 = z
       x = test1 if test else test2.

       We need to use different variables for test1 and test2 to preserve
       the guarantee that every variable is bound at most once. This means
       that one branch of the if _expression_ will always result in an
       unbound variable error. It is guaranteed that this is the branch we
       do not run, but this still makes me sad.
    *)
    let test_bindings, test_result = normalize_expr test in
    let body_bindings, body_result = normalize_expr body in
    let orelse_bindings, orelse_result = normalize_expr orelse in
    let big_if =
      Normalized.If(test_result, body_bindings, orelse_bindings,
                    get_next_uid annot) in
    let assignment, name = gen_normalized_assignment annot
        (Normalized.IfExp(test_result, body_result, orelse_result,
                          get_next_uid annot)) in
    test_bindings @ [big_if] @ assignment, name

  | Abstract.Compare (left, ops, comparators, annot) ->
    (* "x < y < z" is equivalent to "x < y and y < z", except y is only
       evaluated once. We treat compare in almost exactly the same way
       as we treat boolean operators (see above), but we also keep track
       of the name we used to bind the last expression, rather than just the
       result of the last comparison.
       This lets us re-use the value of y we computed for x < y when we
       compute y < z, which ensures that y is only evaluated once. *)
    let left_bindings, left_result = normalize_expr left in
    let normed_ops = List.map normalize_cmpop ops in
    let combine
        (prev : Normalized.stmt list (* Previous operations *) *
                Normalized.simple_expr (* Result of previous op *) *
                Normalized.simple_expr (* Name of previous expr *))
        (next_op : Normalized.cmpop)
        (next_expr : 'a Abstract.expr) =
      (* Unpack the input tuple *)
      let prev_stmts, prev_result, prev_expr = prev in
      (* Normalize the next expression we might compare against. This
         code will only be executed if the previous result is True.*)
      let bindings, curr_expr = normalize_expr next_expr in
      (* Actually perform the comparison between the previous expression
         and the one we just normalized.*)
      let interior_assignment, curr_result = gen_normalized_assignment annot
          (Normalized.Compare(prev_expr,
                              next_op,
                              curr_expr,
                              get_next_uid annot)) in
      let big_if =
        Normalized.If(prev_result,
                      bindings @ interior_assignment,
                      [Normalized.Assign(
                          curr_result,
                          Normalized.SimpleExpr(
                            Normalized.Bool(false, get_next_uid annot),
                            get_next_uid annot),
                          get_next_uid annot)
                      ],
                      get_next_uid annot) in
      (* Generate our next value of "prev_result" *)
      let exterior_assignment, next_result = gen_normalized_assignment annot
          (Normalized.BoolOp(prev_result,
                             Normalized.And,
                             curr_result, get_next_uid annot)) in
      (prev_stmts @ [big_if] @ exterior_assignment),
      next_result,
      curr_expr
    in (* End definition of combine *)
    let stmts, result, _ =
      List.fold_left2 combine
        (
          left_bindings,
          Normalized.Bool(true, get_next_uid annot),
          left_result
        )
        normed_ops
        comparators
    in
    stmts, result

  | Abstract.Call (func, args, _, _, _, annot) ->
    let func_bindings, func_name = normalize_expr func in
    let arg_bindings, arg_names = normalize_expr_list args in
    let assignment, name = gen_normalized_assignment annot
        (Normalized.Call(func_name, arg_names, get_next_uid annot)) in
    let bindings = func_bindings @ arg_bindings @ assignment in
    bindings, name

  | Abstract.Num (n, annot) ->
    ([], Normalized.Num(normalize_number n, get_next_uid annot))

  | Abstract.Str (annot) ->
    ([], Normalized.Str(get_next_uid annot))

  | Abstract.Bool (b, annot) ->
    ([], Normalized.Bool(b, get_next_uid annot))

  | Abstract.Attribute (obj, attr, _, annot) ->
    let obj_bindings, obj_result = normalize_expr obj in
    let assignment, result = gen_normalized_assignment annot
        (Normalized.Attribute(obj_result, attr, get_next_uid annot)) in
    obj_bindings @ assignment, result

  | Abstract.Subscript (value, slice, _, annot) ->
    let value_bindings, value_result = normalize_expr value in
    let slice_bindings, slice_result = normalize_slice slice in
    let assignment, name =
      gen_normalized_assignment annot (Normalized.Subscript(value_result,
                                                            slice_result,
                                                            get_next_uid annot)) in
    let bindings = value_bindings @ slice_bindings @ assignment in
    bindings, name

  | Abstract.Name (id, _, annot) -> (* Throw out context *)
    ([], Normalized.Name(id, get_next_uid annot))

  | Abstract.List (elts, _, annot) ->
    let bindings, results = normalize_expr_list elts in
    let assignment, name =
      gen_normalized_assignment annot
        (Normalized.List(results, get_next_uid annot)) in
    bindings @ assignment, name

  | Abstract.Tuple (elts, _, annot) ->
    let bindings, results = normalize_expr_list elts in
    let assignment, name =
      gen_normalized_assignment annot
        (Normalized.Tuple(results, get_next_uid annot)) in
    bindings @ assignment, name

(* Given a list of exprs, returns a list containing all of their
   bindings and a list containing all of the relevant variable names *)
and normalize_expr_list lst = normalize_list normalize_expr lst

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
