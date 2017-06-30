open Python2_ast_types
module Simplified = Python2_simplified_ast;;
module Normalized = Python2_normalized_ast;;
open Python2_normalization_ctx;;

(* FIXME: We need to create a type for builtin methods (in addition to builtin
   functions such as type, bool, slice) such as __getattr__, and use that
   instead of just a string whenever we invoke them in this file *)
let map_and_concat (func : 'a -> 'b list) (lst : 'a list) =
  List.concat (List.map func lst)
;;

let normalize_option func o =
  match o with
  | None -> None
  | Some(x) -> Some(func x)
;;

(* Given a uid and an annotated_expr, assigns that expr to a new, unique name.
   Returns the assignment statement (in a list) and the name used *)
let gen_normalized_assignment ctx annot
    (e : Normalized.annotated_expr) =
  let u = get_next_uid ctx annot in
  let name = gen_unique_name ctx annot in
  let assignment = {
    uid = u;
    exception_target = e.exception_target;
    multi = e.multi;
    body = Normalized.Assign(name, e);
  }
  in
  [assignment], name
;;

(* Normalizes the input "test" expr and then calls the builtin bool function
   on it. Returns the bindings to do this and the name of the result. *)
let normalize_and_call_bool ctx annot normalizer annotator test =
  let test_bindings, test_name = normalizer test in
  let builtin_bool_binding, builtin_bool_name =
    gen_normalized_assignment ctx annot @@
    annotator @@
    Normalized.Literal(Normalized.Builtin(Builtin_bool))
  in
  let test_bool_binding, test_bool_name =
    gen_normalized_assignment ctx annot @@
    annotator @@
    (* We don't need to get the __call__ method because here we can
       guarantee that the thing bound in builtin_bool_name is already a
       function *)
    Normalized.Call(builtin_bool_name, [test_name])
  in
  test_bindings @ builtin_bool_binding @ test_bool_binding,
  test_bool_name
;;

let create_annotation_from_stmt ctx exception_target in_loop
    (s : 'a Simplified.stmt) (e : 'b) =
  {
    uid = get_next_uid ctx (Simplified.extract_stmt_annot s);
    exception_target = exception_target;
    multi = in_loop;
    body = e;
  }
;;

let create_annotation_from_expr ctx exception_target in_loop
    (s : 'a Simplified.expr) (e : 'b) =
  {
    uid = get_next_uid ctx (Simplified.extract_expr_annot s);
    exception_target = exception_target;
    multi = in_loop;
    body = e;
  }
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

let rec normalize_modl ctx m : Normalized.modl =
  match m with
  | Simplified.Module (body, annot) ->
    let normalized_prog = map_and_concat (normalize_stmt_full ctx None None None) body in
    let next_uid = get_next_uid ctx annot in
    Normalized.Module(normalized_prog, next_uid)

and normalize_stmt_full
    ctx
    loop_start_uid
    loop_end_uid
    exception_target
    (s : 'a Simplified.stmt)
  : Normalized.annotated_stmt list =
  let normalize_expr =
    normalize_expr_full ctx loop_start_uid loop_end_uid exception_target
  in
  let in_loop = (loop_start_uid <> None) in
  (* We need some additional arguments when we're inside a loop,
     so that Break and Continue know what to do. These are only neded in that
     special case, though, so it's convenient to hide them *)
  let normalize_stmt ctx e s =
    normalize_stmt_full ctx loop_start_uid loop_end_uid e s in

  let annotate_stmt e : Normalized.annotated_stmt =
    create_annotation_from_stmt ctx exception_target in_loop s e in
  let annotate_expr e : Normalized.annotated_expr =
    create_annotation_from_stmt ctx exception_target in_loop s e in

  match s with
  | Simplified.Return (value, _) ->
    let bindings, result = normalize_expr value in
    bindings @
    [annotate_stmt @@ Normalized.Return(result)]


  | Simplified.Assign (target, value, _) ->
    let value_bindings, value_result = normalize_expr value in
    let assign = annotate_stmt @@
      Normalized.Assign(
        target,
        annotate_expr @@ Normalized.Name(value_result)
      )
    in
    value_bindings @ [assign]

  | Simplified.Print (dest, values, nl, _) ->
    let dest_bindings, dest_result = normalize_expr_option normalize_expr dest in
    let value_bindings, value_results = normalize_list normalize_expr values in
    let bindings = dest_bindings @ value_bindings in
    let print = annotate_stmt @@
      Normalized.Print(dest_result,
                       value_results,
                       nl)
    in
    bindings @ [print]

  | Simplified.While (test, body, annot) ->
    (* We want to always mark stmts created here as possible executed multiple
       times, so override annotate_* to do that for us *)
    let annotate_stmt e : Normalized.annotated_stmt =
      create_annotation_from_stmt ctx exception_target true s e in
    let annotate_expr e : Normalized.annotated_expr =
      create_annotation_from_stmt ctx exception_target true s e in

    let start_stmt = annotate_stmt @@ Normalized.Pass in
    let start_uid = start_stmt.uid in (* Start label *)
    let end_stmt = annotate_stmt @@ Normalized.Pass in
    let end_uid = end_stmt.uid in (* End label *)

    let test_bindings, test_name =
      normalize_and_call_bool ctx annot
        (normalize_expr_full ctx (Some start_uid) (Some end_uid) exception_target)
        annotate_expr
        test
    in

    let normalized_body =
      (map_and_concat
         (normalize_stmt_full ctx
            (Some(start_uid))
            (Some(end_uid))
            exception_target)
         body)
    in
    let run_test = annotate_stmt @@
      Normalized.GotoIfNot(test_name, end_uid)
    in
    let end_stmts =
      [
        annotate_stmt @@ Normalized.Goto(start_uid);
        end_stmt;
      ]
    in
    [start_stmt] @
    test_bindings @
    [run_test] @
    normalized_body @
    end_stmts

  (* Turn "if test: <body> else: <orelse>" into
     "if not test: goto orelse
     <body>
     goto end
     label orelse
     <orelse>
     label end"
  *)
  | Simplified.If (test, body, orelse, annot) ->
    let test_bindings, test_name =
      normalize_and_call_bool ctx annot
        normalize_expr
        annotate_expr
        test
    in

    let normalized_body =
      map_and_concat (normalize_stmt ctx exception_target) body in
    let normalized_orelse =
      map_and_concat (normalize_stmt ctx exception_target) orelse
    in

    let end_label = annotate_stmt @@ Normalized.Pass in
    let end_uid = end_label.uid in
    let goto_end_label = annotate_stmt @@ Normalized.Goto(end_uid) in
    let orelse_label = annotate_stmt @@ Normalized.Pass in
    let orelse_uid = orelse_label.uid in

    test_bindings @
    [annotate_stmt @@ Normalized.GotoIfNot(test_name, orelse_uid)] @
    normalized_body @
    [
      goto_end_label;
      orelse_label
    ] @
    normalized_orelse @
    [end_label]

  | Simplified.Raise (value, _) ->
    let value_binding, value_result = normalize_expr value in
    value_binding @
    [annotate_stmt @@ Normalized.Raise(value_result)]

  | Simplified.TryExcept (body, handlers, annot) ->
    let exception_name = gen_unique_name ctx annot in
    let catch = annotate_stmt @@ Normalized.Catch(exception_name) in
    let handler_start_uid = catch.uid in
    let handler_end_stmt = annotate_stmt Normalized.Pass in
    let handler_end_uid = handler_end_stmt.uid in

    let normalized_body =
      map_and_concat (normalize_stmt ctx (Some(handler_start_uid))) body in

    let rec handlers_to_simplified_if handler_list =
      match handler_list with
      | [] -> (* If we run out of handlers, re-raise the current exception *)
        Simplified.Raise(Simplified.Name(exception_name, annot), annot)

      | Simplified.ExceptHandler(typ, name, body, annot)::rest ->
        let if_test = (* Check if the exception matches this type *)
          match typ with
          | None ->
            Simplified.Bool(true, annot)
          | Some(exp) ->
            Simplified.Compare(
              Simplified.Call(
                Simplified.Builtin(Builtin_type, annot),
                [Simplified.Name(exception_name, annot)],
                annot),
              [Simplified.Is],
              [exp],
              annot)
        in
        let bind_exception =
          (* Bind the exception to the given name, if we have one *)
          match name with
          | None -> []
          | Some(id) ->
            [Simplified.Assign(
                id,
                Simplified.Name(exception_name, annot),
                annot)]
        in
        Simplified.If(
          if_test,
          bind_exception @ body,
          [
            handlers_to_simplified_if rest
          ],
          annot)
    in (* End handler_to_simplified_if definition *)

    let normalized_handlers =
      (normalize_stmt ctx exception_target)
        (handlers_to_simplified_if handlers)
    in
    let handler_body =
      [catch] @
      normalized_handlers @
      [handler_end_stmt]
    in
    normalized_body @
    [annotate_stmt @@ Normalized.Goto(handler_end_uid)] @
    handler_body

  | Simplified.Pass _ ->
    [annotate_stmt @@ Normalized.Pass]

  | Simplified.Break _ ->
    begin
      match loop_end_uid with
      | None -> failwith "'break' outside loop"
      | Some(u) -> [annotate_stmt @@ Normalized.Goto(u)]
    end

  | Simplified.Continue _ ->
    begin
      match loop_start_uid with
      | None -> failwith "'continue' not properly in loop"
      | Some(u) -> [annotate_stmt @@ Normalized.Goto(u)]
    end

  | Simplified.Expr (e, annot) ->
    let bindings, _ =
      match e with
      (* If this is just a name, we generate a useless assignment so that
         we can fit it into an assignment statement format. For all other
         exprs, normalize_expr will do this for us *)
      | Simplified.Name (id, _) ->
        gen_normalized_assignment ctx annot @@
        annotate_expr @@ Normalized.Name(id)
      | _ ->
        normalize_expr e
    in
    bindings

(* Given a simplified expr, returns a list of statements, corresponding to
   the assignments necessary to compute it, and the name of the final
   variable that was bound *)
and normalize_expr_full
    ctx
    loop_start_uid
    loop_end_uid
    exception_target
    (e : 'a Simplified.expr)
  : Normalized.annotated_stmt list * identifier =
  let normalize_stmt =
    normalize_stmt_full ctx loop_start_uid loop_end_uid exception_target
  in
  let normalize_expr =
    normalize_expr_full ctx loop_start_uid loop_end_uid exception_target
  in
  let in_loop = loop_start_uid <> None in
  let annotate_stmt ex : Normalized.annotated_stmt =
    create_annotation_from_expr ctx exception_target in_loop e ex in
  let annotate_expr ex : Normalized.annotated_expr =
    create_annotation_from_expr ctx exception_target in_loop e ex in
  match e with
  (* BoolOps are a tricky case because of short-circuiting. We need to
     make sure that when we evaluate "a and False and b", b is never
     evaluated, etc.

     We do this by iteratively breaking down the statements like so:
     "a and b and c and ..." turns into

     "if a then (b and c and ...) else a", except we make sure that
     a is only evaluated once by storing a in a tmp variable after it's
     evaluated.

     To avoid duplicate code, we construct a simplified IfExp to represent
     the above code, then recursively simplify that.
  *)
  | Simplified.BoolOp (op, operands, annot) ->
    begin
      match operands with
      | [] -> failwith "No arguments to BoolOp"
      | hd::[] -> normalize_expr hd
      | hd::tl ->
        let test_bindings, test_result = (normalize_expr hd) in

        let if_exp =
          begin
            match op with
            | Simplified.And ->
              Simplified.IfExp(
                Simplified.Name(test_result, annot),
                Simplified.BoolOp (op, tl, annot),
                Simplified.Name(test_result, annot),
                annot)
            | Simplified.Or ->
              Simplified.IfExp(
                Simplified.Name(test_result, annot),
                Simplified.Name(test_result, annot),
                Simplified.BoolOp (op, tl, annot),
                annot)
          end
        in
        let bindings, result =
          normalize_expr if_exp
        in
        test_bindings @ bindings, result

    end

  | Simplified.IfExp (test, body, orelse, annot) ->
    (* Python allows expressions like x = 1 if test else 0. Of course,
       only the relevant branch is executed, so we can't simply evaluate both
       beforehand. But in order to be on the right-hand-side of an assignment,
       the expression must be no more complicated than a compound_expr. In
       particular, the expression can't be an assignment.

       So to evaluate "x = y if test else z", we first create an if _statement_
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
    let tmp_name = gen_unique_name ctx annot in
    let test_bindings, test_name =
      normalize_and_call_bool ctx annot
        normalize_expr
        annotate_expr
        test
    in
    let body_bindings, body_result = normalize_expr body in
    let body_bindings_full =
      body_bindings @ [
        annotate_stmt @@
        Normalized.Assign(tmp_name,
                          annotate_expr @@ Normalized.Name(body_result))
      ] in
    let orelse_bindings, orelse_result = normalize_expr orelse in
    let orelse_bindings_full =
      orelse_bindings @ [
        annotate_stmt @@
        Normalized.Assign(tmp_name,
                          annotate_expr @@ Normalized.Name(orelse_result))
      ] in
    let end_stmt = annotate_stmt @@ Normalized.Pass in
    let end_uid = end_stmt.uid in
    let goto_end_stmt = annotate_stmt @@ Normalized.Goto(end_uid) in
    let orelse_stmt = annotate_stmt @@ Normalized.Pass in
    let orelse_uid = orelse_stmt.uid in

    let run_test = annotate_stmt @@
      Normalized.GotoIfNot(test_name, orelse_uid)
    in

    test_bindings @
    [run_test] @
    body_bindings_full @
    [
      goto_end_stmt;
      orelse_stmt;
    ] @
    orelse_bindings_full @
    [end_stmt],
    tmp_name

  | Simplified.Compare (left, ops, comparators, annot) ->
    (* "x < y < z" is equivalent to "x < y and y < z", except y is only
       evaluated once. We treat compare in almost exactly the same way
       as we treat boolean operators.

       Specifically, we turn "x < y < z < ..."

       into

       tmp1 = x
       tmp2 = y
       tmp3 = tmp1.__lt__
       tmp4 = tmp3(tmp2)
       if tmp4 then (tmp2 < z < ...) else tmp4*)

    (* Helper function that takes a variable name, then generates normalized
       code that inverts it *)
    let invert id =
      let inverted_name = gen_unique_name ctx annot in
      let inverter =
        normalize_stmt_full ctx loop_start_uid loop_end_uid exception_target @@
        Simplified.Assign(inverted_name,
                          Simplified.IfExp(Simplified.Name(id, annot),
                                           Simplified.Bool(false, annot),
                                           Simplified.Bool(true, annot),
                                           annot),
                          annot)
      in
      inverter, inverted_name
    in

    let left_bindings, left_result = normalize_expr left in
    begin
      match ops with
      | [] -> failwith "No operation given to comparison"
      | hd::tl ->
        let right_bindings, right_result =
          normalize_expr (List.hd comparators) in
        let cmp_bindings, cmp_result =
          match hd with
          | Simplified.Is ->
            gen_normalized_assignment ctx annot @@
            annotate_expr @@
            Normalized.Binop(left_result, Normalized.Binop_is, right_result)

          | Simplified.IsNot ->
            let cmp_bindings, cmp_result =
              gen_normalized_assignment ctx annot @@
              annotate_expr @@
              Normalized.Binop(left_result, Normalized.Binop_is, right_result)
            in
            let inv_bindings, inv_result = invert cmp_result in
            cmp_bindings @ inv_bindings,
            inv_result

          | Simplified.NotIn ->
            let cmp_func_bindings, cmp_func_result =
              normalize_expr @@
              Simplified.Attribute(Simplified.Name(left_result, annot),
                                   normalize_cmpop Simplified.In,
                                   annot)
            in
            let cmp_bindings, cmp_result =
              normalize_expr @@
              Simplified.Call(Simplified.Name(cmp_func_result, annot),
                              [Simplified.Name(right_result, annot)],
                              annot)
            in
            let inv_bindings, inv_result = invert cmp_result in
            cmp_func_bindings @ cmp_bindings @ inv_bindings,
            inv_result

          | _ ->
            let cmp_func_bindings, cmp_func_result =
              normalize_expr @@
              Simplified.Attribute(Simplified.Name(left_result, annot),
                                   normalize_cmpop hd,
                                   annot)
            in
            let cmp_bindings, cmp_result =
              normalize_expr @@
              Simplified.Call(Simplified.Name(cmp_func_result, annot),
                              [Simplified.Name(right_result, annot)],
                              annot)
            in
            let all_bindings = cmp_func_bindings @
                               cmp_bindings
            in
            all_bindings, cmp_result
        in
        let all_bindings = left_bindings @ right_bindings @ cmp_bindings in
        begin
          match tl with
          | [] -> all_bindings, cmp_result
          | _ ->
            let if_exp =
              Simplified.IfExp(
                Simplified.Name(cmp_result, annot),
                Simplified.Compare(Simplified.Name(right_result, annot),
                                   tl,
                                   List.tl comparators,
                                   annot),
                Simplified.Name(cmp_result, annot),
                annot)
            in
            let bindings, result = normalize_expr if_exp in
            all_bindings @ bindings, result
        end
    end

  | Simplified.Call (func, args, annot) ->
    (* In order to call an object, we must first get its __call__ attribute.
       If this does not exist, we throw a type error. Otherwise, we continue
       to retrieve the __call__ attribute until we get a method wrapper,
       which we know how to call. *)
    let func_bindings, func_name = normalize_expr func in
    let arg_bindings, arg_names = normalize_list normalize_expr args in
    let get_call_bindings, callable_name =
      match func with
      | Simplified.Builtin _ -> [], func_name
      | Simplified.Name ("*get_call", _) -> [], func_name
      | _ ->
        let callable_name = gen_unique_name ctx annot in
        let get_call_call =
          Simplified.Assign(
            callable_name,
            Simplified.Call(Simplified.Name("*get_call", annot),
                            [Simplified.Name(func_name, annot)],
                            annot),
            annot)
        in
        normalize_stmt get_call_call, callable_name
    in
    let assignment, name = gen_normalized_assignment ctx annot @@
      annotate_expr @@
      Normalized.Call(callable_name, arg_names) in
    let bindings = func_bindings @ arg_bindings @ get_call_bindings @ assignment in
    bindings, name

  | Simplified.Num (n, annot) ->
    gen_normalized_assignment ctx annot @@
    annotate_expr @@
    Normalized.Literal(Normalized.Num(n))

  | Simplified.Str (s, annot) ->
    gen_normalized_assignment ctx annot @@
    annotate_expr @@
    Normalized.Literal(Normalized.Str(s))

  | Simplified.Bool (b, annot) ->
    gen_normalized_assignment ctx annot @@
    annotate_expr @@
    Normalized.Literal(Normalized.Bool(b))

  | Simplified.Builtin (b, annot) ->
    gen_normalized_assignment ctx annot @@
    annotate_expr @@
    Normalized.Literal(Normalized.Builtin(b))

  | Simplified.FunctionVal (args, body, annot) ->
    let normalized_body = List.concat @@
      List.map (normalize_stmt_full ctx None None exception_target) body in
    gen_normalized_assignment ctx annot @@
    annotate_expr @@
    Normalized.Literal(Normalized.FunctionVal(args, normalized_body))

  | Simplified.Attribute (obj, attr, annot) ->
    (* Attribute lookups follow a rather complicated process. We first look for
       a __getattribute__ method (new-style classes only), which is called first.
       If it raises an AttributeError, we call __getattr__ instead.
       We retrieve __getattribute__ and __getattr__ in a special way, which we
       represent here using the SimpleAttribute constructor.

       The resulting code for "obj.member" looks something like this:
       try:
         tmp1 = obj.__getattribute__
         result = tmp1("member")
       except AttributeError as e:
         try:
           tmp2 = obj.__getattr__
         except AttributeError:
           raise e
         result = tmp2("member")

       We then return result. The '.' operator here is the SimpleAttribute
       described below.
    *)
    let obj_bindings, obj_result = normalize_expr obj in
    let obj_name = Simplified.Name(obj_result, annot) in
    let getattribute_name = gen_unique_name ctx annot in
    let getattr_name = gen_unique_name ctx annot in
    let exn_name = gen_unique_name ctx annot in
    let getattr_try =
      Simplified.TryExcept(
        [
          Simplified.Assign(getattr_name,
                            Simplified.SimpleAttribute(obj_name, "__getattr__", annot),
                            annot);
        ],
        [
          Simplified.ExceptHandler(
            Some(Simplified.Builtin(Builtin_AttributeError, annot)),
            None,
            [ Simplified.Raise(Simplified.Name(exn_name, annot), annot) ],
            annot);
        ],
        annot)
    in
    let result_name = gen_unique_name ctx annot in
    let overall_try =
      Simplified.TryExcept(
        [
          (* Simplified.Assign(result_name,
                            Simplified.SimpleAttribute(obj_name, attr, annot),
                            annot); *)
          Simplified.Assign(getattribute_name,
                            Simplified.SimpleAttribute(obj_name, "__getattribute__", annot),
                            annot);
          Simplified.Assign(result_name,
                            Simplified.Call(Simplified.Name(getattribute_name, annot),
                                            [Simplified.Str(StringLiteral(attr), annot)],
                                            annot),
                            annot);
        ],
        [
          Simplified.ExceptHandler(
            Some(Simplified.Builtin(Builtin_AttributeError, annot)),
            Some(exn_name),
            [
              getattr_try;
              Simplified.Assign(result_name,
                                Simplified.Call(
                                  Simplified.Name(getattr_name, annot),
                                  [Simplified.Str(StringLiteral(attr), annot)],
                                  annot),
                                annot);
            ],
            annot);
        ],
        annot)
    in
    let normalized_try = normalize_stmt overall_try in
    obj_bindings @ normalized_try, result_name

  | Simplified.SimpleAttribute (obj, attr, annot)
  (* TODO Once classes are implemented, we need to look for the attribute
     in the instance, then the class and all of its parents. This should look
     something like
     tmp = obj
     while obj != None:
       try:
         result = tmp.member # This . is the ImmediateAttribute below
         break
       except AttributeError:
         obj = getparent(obj)
     if (obj == None):
       raise AttributeError

     And we then return result. For the moment, we only look at the instance,
     and so fall through to the next case. *)

  | Simplified.ImmediateAttribute (obj, attr, annot) ->
    let obj_bindings, obj_result = normalize_expr obj in
    let assignment, result = gen_normalized_assignment ctx annot @@
      annotate_expr @@
      Normalized.Attribute(obj_result, attr)
      (* NOTE: This is the only place the Normalized.Attribute constructor should show up. *)
    in
    obj_bindings @ assignment, result

  | Simplified.Name (id, _) ->
    ([], id)

  | Simplified.List (elts, annot) ->
    let bindings, results = normalize_list normalize_expr elts in
    let assignment, name =
      gen_normalized_assignment ctx annot @@
      annotate_expr @@ Normalized.List(results) in
    bindings @ assignment, name

  | Simplified.Tuple (elts, annot) ->
    let bindings, results = normalize_list normalize_expr elts in
    let assignment, name =
      gen_normalized_assignment ctx annot @@
      annotate_expr @@ Normalized.Tuple(results) in
    bindings @ assignment, name

and normalize_expr_option func
    (o : 'a Simplified.expr option)
  : Normalized.annotated_stmt list * identifier option =
  let normalized_opt = normalize_option func o in
  match normalized_opt with
  | None -> [], None
  | Some(bindings, result) -> bindings, Some(result)

and normalize_cmpop o =
  match o with
  | Simplified.Eq -> "__eq__"
  | Simplified.NotEq -> "__ne__"
  | Simplified.Lt -> "__lt__"
  | Simplified.LtE -> "__le__"
  | Simplified.Gt -> "__gt__"
  | Simplified.GtE -> "__ge__"
  | Simplified.In -> "__contains__"
  | _ -> failwith "Tried to normalize an invalid cmpop!"
