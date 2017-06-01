module Simplified = Python2_simplified_ast
module Normalized = Python2_normalized_ast

let get_next_uid _ = 0;;

let name_counter = ref 0;;

let gen_unique_name _ =
  let count = !name_counter in
  name_counter := count + 1;
  "$normalized_unique_name_" ^ string_of_int count
;;

let reset_unique_name () = name_counter := 0;;

let map_and_concat (func : 'a -> 'b list) (lst : 'a list) =
  List.concat (List.map func lst)
;;

let normalize_option func o =
  match o with
  | None -> None
  | Some(x) -> Some(func x)
;;

let update_uid annot (e : Normalized.simple_expr) =
  match e with
  | Normalized.Num (n, _) -> Normalized.Num(n, get_next_uid annot)
  | Normalized.Str (s, _) -> Normalized.Str(s, get_next_uid annot)
  | Normalized.Bool (b, _) -> Normalized.Bool(b, get_next_uid annot)
  | Normalized.Name (id, _) -> Normalized.Name(id, get_next_uid annot)
;;

let update_option_uid annot (opt: Normalized.simple_expr option) =
  match opt with
  | None -> None
  | Some(e) -> Some(update_uid annot e)
;;

let id_of_name n =
  match n with
  | Normalized.Name (id, _) -> id
  | _ -> failwith "Can only extract id from names"
;;

(* Given a uid and a compound_expr, assigns that expr to a new, unique name.
   Returns the assignment statement (in a list) and the name used *)
let gen_normalized_assignment annot e =
  let u = get_next_uid annot in
  let name = gen_unique_name u in
  let assignment = Normalized.Assign(name, e, u) in
  [assignment], Normalized.Name(name, get_next_uid annot)
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
  | Simplified.Module (body, annot) ->
    Normalized.Module(map_and_concat normalize_stmt body, get_next_uid annot)

(* We need some additional arguments when we're inside a loop,
   so that Break and Continue know what to do. These are only neded in that
   special case, though, so it's convenient to hide them in other cases. *)
and normalize_stmt s = normalize_stmt_full None None s

and normalize_stmt_full
    loop_start_uid
    loop_end_uid
    (s : 'a Simplified.stmt)
  : Normalized.stmt list =
  match s with
  | Simplified.FunctionDef (func_name, args, body, annot)->
    (* Hopefully the args are just names so this won't do anything *)
    let normalized_body = map_and_concat normalize_stmt body in
    [Normalized.FunctionDef(func_name,
                            args,
                            normalized_body,
                            get_next_uid annot)]

  | Simplified.Return (value, annot) ->
    begin
      match value with
      | None -> [Normalized.Return(None, get_next_uid annot)]
      | Some(x) ->
        let bindings, result = normalize_expr x in
        bindings @
        [Normalized.Return(
            Some(update_uid annot result),
            get_next_uid annot)]
    end


  | Simplified.Assign (target, value, annot) ->
    let value_bindings, value_result = normalize_expr value in
    value_bindings @
    [Normalized.Assign(
        target,
        Normalized.SimpleExpr(
          update_uid annot value_result,
          get_next_uid annot),
        get_next_uid annot)]

  | Simplified.Print (dest, values, nl, annot) ->
    let dest_bindings, dest_result = normalize_expr_option dest in
    let value_bindings, value_results = normalize_expr_list values in
    let bindings = dest_bindings @ value_bindings in
    bindings @ [Normalized.Print(update_option_uid annot dest_result,
                                 List.map (update_uid annot) value_results,
                                 nl,
                                 get_next_uid annot)]

  | Simplified.While (test, body, annot) ->
    let test_bindings, test_name =
      (* It's nicer to have "if !test then goto end" as opposed to
         "if test then pass else goto end" *)
      normalize_expr (Simplified.UnaryOp(Simplified.Not,
                                         test,
                                         annot)) in

    let start_uid = get_next_uid annot in (* Start label *)
    let end_uid = get_next_uid annot in (* End label *)
    let test_at_beginning =
      Normalized.If(update_uid annot test_name,
                    [Normalized.Goto(end_uid, get_next_uid annot)],
                    [],
                    get_next_uid annot) in
    let normalized_body =
      (map_and_concat
         (normalize_stmt_full
            (Some(start_uid))
            (Some(end_uid)))
         body) in
    let start_stmt = Normalized.Pass(start_uid) in
    let end_stmts =
      [
        Normalized.Goto(start_uid, get_next_uid annot);
        Normalized.Pass(end_uid);
      ] in
    [start_stmt] @
    test_bindings @
    [test_at_beginning] @
    normalized_body @
    end_stmts

  | Simplified.If (test, body, orelse, annot) ->
    let test_bindings, test_name = normalize_expr test in
    let normalized_body = map_and_concat normalize_stmt body in
    let normalized_orelse = map_and_concat normalize_stmt orelse in
    test_bindings @
    [Normalized.If(update_uid annot test_name,
                   normalized_body,
                   normalized_orelse,
                   get_next_uid annot)]

  | Simplified.Raise (value, annot) ->
    let value_binding, value_result = normalize_expr value in
    value_binding @
    [Normalized.Raise(update_uid annot value_result,
                      get_next_uid annot)]

  | Simplified.TryExcept _ -> [] (* TODO *)
  (* TODO/FIXME: ExceptHandlers need to have expr options as their first type *)

  | Simplified.Pass (annot) ->
    [Normalized.Pass (get_next_uid annot)]

  | Simplified.Break (annot) ->
    begin
      match loop_end_uid with
      | None -> failwith "'break' outside loop"
      | Some(u) -> [Normalized.Goto(u, get_next_uid annot)]
    end

  | Simplified.Continue (annot) ->
    begin
      match loop_start_uid with
      | None -> failwith "'continue' not properly in loop"
      | Some(u) -> [Normalized.Goto(u, get_next_uid annot)]
    end

  | Simplified.Expr (e, annot) ->
    let bindings, result = normalize_expr e in
    bindings @ [Normalized.SimpleExprStmt(update_uid annot result,
                                          get_next_uid annot)]

(* Given a simplified expr, returns a list of statements, corresponding to
   the assignments necessary to compute it, and the name of the final
   variable that was bound *)
and normalize_expr
    (e : 'a Simplified.expr)
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
  | Simplified.BoolOp (op, values, annot) ->
    (* TODO: Throw a useful error if values is empty. Not sure if that's
       possible given how we generate these trees, but best to be safe. *)
    let first_arg = List.hd values in
    let first_arg_bindings, first_arg_result = normalize_expr first_arg in
    let remaining_args = List.tl values in
    let norm_op = normalize_boolop op in
    (* Performs the single step of decomposition described above *)
    let combine
        (prev : Normalized.stmt list * Normalized.simple_expr)
        (next : 'a Simplified.expr)
      : Normalized.stmt list * Normalized.simple_expr =
      let next_tmp_name = gen_unique_name annot in
      let bindings, result = normalize_expr next in
      let body, orelse =
        match op with
        | Simplified.And ->
          bindings @ [
            Normalized.Assign(
              next_tmp_name,
              Normalized.SimpleExpr(update_uid annot result,
                                    get_next_uid annot),
              get_next_uid annot)],

          [Normalized.Assign(
              next_tmp_name,
              Normalized.SimpleExpr(
                Normalized.Bool(false, get_next_uid annot),
                get_next_uid annot),
              get_next_uid annot)]

        | Simplified.Or ->
          [Normalized.Assign(
              next_tmp_name,
              Normalized.SimpleExpr(
                Normalized.Bool(true, get_next_uid annot),
                get_next_uid annot),
              get_next_uid annot)],

          bindings @ [
            Normalized.Assign(
              next_tmp_name,
              Normalized.SimpleExpr(update_uid annot result,
                                    get_next_uid annot),
              get_next_uid annot)]
      in
      let big_if = Normalized.If(snd prev, body, orelse, get_next_uid annot) in
      let assignment, final_tmp_name =
        gen_normalized_assignment annot
          (Normalized.BoolOp(
              snd prev,
              norm_op,
              Normalized.Name(next_tmp_name, get_next_uid annot),
              get_next_uid annot)) in
      let bindings = (fst prev) @ [big_if] @ assignment in
      bindings, final_tmp_name
    in (* End definition of combine *)
    List.fold_left combine
      (first_arg_bindings, update_uid annot first_arg_result)
      remaining_args

  | Simplified.BinOp (left, op, right, annot) ->
    let left_bindings, left_result = normalize_expr left in
    let right_bindings, right_result = normalize_expr right in
    let assignment, name =
      gen_normalized_assignment
        annot
        (Normalized.BinOp(update_uid annot left_result,
                          normalize_operator op,
                          update_uid annot right_result,
                          get_next_uid annot)) in
    let bindings = left_bindings @ right_bindings @ assignment in
    bindings, name

  | Simplified.UnaryOp (op, operand, annot) ->
    let bindings, result = normalize_expr operand in
    let assignment, name = gen_normalized_assignment annot
        (Normalized.UnaryOp(normalize_unaryop op,
                            update_uid annot result,
                            get_next_uid annot)) in
    bindings @ assignment, name

  | Simplified.IfExp (test, body, orelse, annot) ->
    (* Python allows expressions like x = 1 if test else 0. Of course,
       only the relevant branch is executed, so we can't simply evaluate both
       beforehand. But in order to be on the right-hand-side of an assignment,
       the expression must be no more complicated than a compound_expr. In
       particular, the expression can't be an assignment.

       So to evaluate x = y if test else z, we first create an if _statement_
       and then use the results of that.
       if test:
         # evaluate and bind tmp = y
       else:
         # evaluate and bind tmp = z
       x = tmp.

       We need to use different variables for test1 and test2 to preserve
       the guarantee that every variable is bound at most once. This means
       that one branch of the if _expression_ will always result in an
       unbound variable error. It is guaranteed that this is the branch we
       do not run, but this still makes me sad.
    *)
    let tmp_name = gen_unique_name annot in
    let test_bindings, test_result = normalize_expr test in
    let body_bindings, body_result = normalize_expr body in
    let body_bindings_full =
      body_bindings @ [
        Normalized.Assign(tmp_name,
                          Normalized.SimpleExpr(
                            update_uid annot body_result,
                            get_next_uid annot),
                          get_next_uid annot)
      ] in
    let orelse_bindings, orelse_result = normalize_expr orelse in
    let orelse_bindings_full =
      orelse_bindings @ [
        Normalized.Assign(tmp_name,
                          Normalized.SimpleExpr(
                            update_uid annot orelse_result,
                            get_next_uid annot),
                          get_next_uid annot)
      ] in
    let big_if =
      Normalized.If(update_uid annot test_result,
                    body_bindings_full,
                    orelse_bindings_full,
                    get_next_uid annot) in

    test_bindings @ [big_if],
    Normalized.Name(tmp_name, get_next_uid annot)

  | Simplified.Compare (left, ops, comparators, annot) ->
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
        (next_expr : 'a Simplified.expr) =
      (* Unpack the input tuple *)
      let prev_stmts, prev_result, prev_expr = prev in
      (* Normalize the next expression we might compare against. This
         code will only be executed if the previous result is True.*)
      let bindings, curr_expr = normalize_expr next_expr in
      (* Actually perform the comparison between the previous expression
         and the one we just normalized.*)
      let interior_assignment, curr_result = gen_normalized_assignment annot
          (Normalized.Compare(update_uid annot prev_expr,
                              next_op,
                              update_uid annot curr_expr,
                              get_next_uid annot)) in
      let big_if =
        Normalized.If(update_uid annot prev_result,
                      bindings @ interior_assignment,
                      [Normalized.Assign(
                          id_of_name curr_result,
                          Normalized.SimpleExpr(
                            Normalized.Bool(false, get_next_uid annot),
                            get_next_uid annot),
                          get_next_uid annot)
                      ],
                      get_next_uid annot) in
      (* Generate our next value of "prev_result" *)
      let exterior_assignment, next_result = gen_normalized_assignment annot
          (Normalized.BoolOp(update_uid annot prev_result,
                             Normalized.And,
                             update_uid annot curr_result,
                             get_next_uid annot)) in
      (prev_stmts @ [big_if] @ exterior_assignment),
      next_result,
      update_uid annot curr_expr
    in (* End definition of combine *)
    let stmts, result, _ =
      List.fold_left2 combine
        (
          left_bindings,
          Normalized.Bool(true, get_next_uid annot),
          update_uid annot left_result
        )
        normed_ops
        comparators
    in
    stmts, (update_uid annot result)

  | Simplified.Call (func, args, annot) ->
    let func_bindings, func_name = normalize_expr func in
    let arg_bindings, arg_names = normalize_expr_list args in
    let assignment, name = gen_normalized_assignment annot
        (Normalized.Call(update_uid annot func_name,
                         List.map (update_uid annot) arg_names,
                         get_next_uid annot)) in
    let bindings = func_bindings @ arg_bindings @ assignment in
    bindings, name

  | Simplified.Num (n, annot) ->
    ([], Normalized.Num(normalize_number n, get_next_uid annot))

  | Simplified.Str (s, annot) ->
    ([], Normalized.Str(normalize_str s, get_next_uid annot))

  | Simplified.Bool (b, annot) ->
    ([], Normalized.Bool(b, get_next_uid annot))

  | Simplified.Attribute (obj, attr, annot) ->
    let obj_bindings, obj_result = normalize_expr obj in
    let assignment, result = gen_normalized_assignment annot
        (Normalized.Attribute(update_uid annot obj_result,
                              attr,
                              get_next_uid annot)) in
    obj_bindings @ assignment, result

  | Simplified.Name (id, annot) ->
    ([], Normalized.Name(id, get_next_uid annot))

  | Simplified.List (elts, annot) ->
    let bindings, results = normalize_expr_list elts in
    let assignment, name =
      gen_normalized_assignment annot
        (Normalized.List(List.map (update_uid annot) results,
                         get_next_uid annot)) in
    bindings @ assignment, name

  | Simplified.Tuple (elts, annot) ->
    let bindings, results = normalize_expr_list elts in
    let assignment, name =
      gen_normalized_assignment annot
        (Normalized.Tuple(List.map (update_uid annot) results,
                          get_next_uid annot)) in
    bindings @ assignment, name

(* Given a list of exprs, returns a list containing all of their
   bindings and a list containing all of the relevant variable names *)
and normalize_expr_list (lst : 'a Simplified.expr list) =
  normalize_list normalize_expr lst

and normalize_expr_option
    (o : 'a Simplified.expr option)
  : Normalized.stmt list * Normalized.simple_expr option =
  let normalized_opt = normalize_option normalize_expr o in
  match normalized_opt with
  | None -> [], None
  | Some(bindings, result) -> bindings, Some(result)

and normalize_boolop b =
  match b with
  | Simplified.And -> Normalized.And
  | Simplified.Or -> Normalized.Or

and normalize_operator o =
  match o with
  | Simplified.Add -> Normalized.Add
  | Simplified.Sub -> Normalized.Sub
  | Simplified.Mult -> Normalized.Mult
  | Simplified.Div -> Normalized.Div
  | Simplified.Mod -> Normalized.Mod
  | Simplified.Pow -> Normalized.Pow

and normalize_unaryop o =
  match o with
  | Simplified.Not -> Normalized.Not
  | Simplified.UAdd -> Normalized.UAdd
  | Simplified.USub -> Normalized.USub

and normalize_cmpop o =
  match o with
  | Simplified.Eq -> Normalized.Eq
  | Simplified.NotEq -> Normalized.NotEq
  | Simplified.Lt -> Normalized.Lt
  | Simplified.LtE -> Normalized.LtE
  | Simplified.Gt -> Normalized.Gt
  | Simplified.GtE -> Normalized.GtE
  | Simplified.In -> Normalized.In
  | Simplified.NotIn -> Normalized.NotIn

and normalize_sign s =
  match s with
  | Simplified.Pos -> Normalized.Pos
  | Simplified.Neg -> Normalized.Neg
  | Simplified.Zero -> Normalized.Zero

and normalize_number n =
  match n with
  | Simplified.Int sgn -> Normalized.Int(normalize_sign sgn)
  | Simplified.Float sgn -> Normalized.Float(normalize_sign sgn)

and normalize_str s =
  match s with
  | Simplified.StringAbstract -> Normalized.StringAbstract
  | Simplified.StringLiteral (s) -> Normalized.StringLiteral (s)
