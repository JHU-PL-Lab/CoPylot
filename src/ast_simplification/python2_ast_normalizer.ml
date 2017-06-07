module Simplified = Python2_simplified_ast;;
module Normalized = Python2_normalized_ast;;
open Uid_generation;;

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

let update_uid ctx annot (e : Normalized.simple_expr) =
  match e with
  | Normalized.Literal (l, _, ex) ->
    begin
      match l with
      | Normalized.Num (n, _, except) ->
        Normalized.Literal(
          Normalized.Num(n, get_next_uid ctx annot, except),
          get_next_uid ctx annot,
          ex)
      | Normalized.Str (s, _, except) ->
        Normalized.Literal(
          Normalized.Str(s, get_next_uid ctx annot, except),
          get_next_uid ctx annot,
          ex)
      | Normalized.Bool (b, _, except) ->
        Normalized.Literal(
          Normalized.Bool(b, get_next_uid ctx annot, except),
          get_next_uid ctx annot,
          ex)
      | Normalized.Builtin (b, _, except) ->
        Normalized.Literal(
          Normalized.Builtin(b, get_next_uid ctx annot, except),
          get_next_uid ctx annot,
          ex)
    end
  | Normalized.Name (id, _, ex) -> Normalized.Name(id, get_next_uid ctx annot, ex)
;;

let update_option_uid ctx annot (opt: Normalized.simple_expr option) =
  match opt with
  | None -> None
  | Some(e) -> Some(update_uid ctx annot e)
;;

let id_of_name n =
  match n with
  | Normalized.Name (id, _, _) -> id
  | _ -> failwith "Can only extract id from names"
;;

(* Given a uid and a compound_expr, assigns that expr to a new, unique name.
   Returns the assignment statement (in a list) and the name used *)
let gen_normalized_assignment ctx exception_target annot e =
  let u = get_next_uid ctx annot in
  let name = gen_unique_name u in
  let assignment = Normalized.Assign(name, e, u, exception_target) in
  [assignment], Normalized.Name(name, get_next_uid ctx annot, exception_target)
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
    Normalized.Module(map_and_concat (normalize_stmt ctx None) body, get_next_uid ctx annot)

(* We need some additional arguments when we're inside a loop,
   so that Break and Continue know what to do. These are only neded in that
   special case, though, so it's convenient to hide them in other cases. *)
and normalize_stmt ctx e s = normalize_stmt_full ctx None None e s

and normalize_stmt_full
    ctx
    loop_start_uid
    loop_end_uid
    exception_target
    (s : 'a Simplified.stmt)
  : Normalized.stmt list =
  match s with
  | Simplified.FunctionDef (func_name, args, body, annot)->
    (* Hopefully the args are just names so this won't do anything *)
    let normalized_body =
      map_and_concat (normalize_stmt ctx exception_target) body in
    [Normalized.FunctionDef(func_name,
                            args,
                            normalized_body,
                            get_next_uid ctx annot,
                            exception_target)]

  | Simplified.Return (value, annot) ->
    begin
      match value with
      | None ->
        [Normalized.Return(
            None,
            get_next_uid ctx annot,
            exception_target)]
      | Some(x) ->
        let bindings, result = normalize_expr ctx exception_target x in
        bindings @
        [Normalized.Return(
            Some(update_uid ctx annot result),
            get_next_uid ctx annot,
            exception_target)]
    end


  | Simplified.Assign (target, value, annot) ->
    let value_bindings, value_result = normalize_expr ctx exception_target value in
    value_bindings @
    [Normalized.Assign(
        target,
        Normalized.SimpleExpr(
          update_uid ctx annot value_result,
          get_next_uid ctx annot,
          exception_target),
        get_next_uid ctx annot,
        exception_target)]

  | Simplified.Print (dest, values, nl, annot) ->
    let dest_bindings, dest_result = normalize_expr_option ctx exception_target dest in
    let value_bindings, value_results = normalize_expr_list ctx exception_target values in
    let bindings = dest_bindings @ value_bindings in
    bindings @ [Normalized.Print(update_option_uid ctx annot dest_result,
                                 List.map (update_uid ctx annot) value_results,
                                 nl,
                                 get_next_uid ctx annot,
                                 exception_target)]

  | Simplified.While (test, body, annot) ->
    let test_bindings, test_name =
      normalize_expr ctx exception_target test
    in
    let test_bool_binding, test_bool_name =
      gen_normalized_assignment ctx exception_target annot
        (Normalized.Call(
            Normalized.Literal(
              Normalized.Builtin(Normalized.Builtin_bool,
                                 get_next_uid ctx annot,
                                 exception_target),
              get_next_uid ctx annot,
              exception_target),
            [update_uid ctx annot test_name],
            get_next_uid ctx annot,
            exception_target))
in

    let start_uid = get_next_uid ctx annot in (* Start label *)
    let end_uid = get_next_uid ctx annot in (* End label *)
    let normalized_body =
      (map_and_concat
         (normalize_stmt_full ctx
            (Some(start_uid))
            (Some(end_uid))
            exception_target)
         body)
    in
    let run_test =
      Normalized.GotoIfNot(update_uid ctx annot test_bool_name,
                           end_uid,
                           get_next_uid ctx annot,
                           exception_target)
    in
    let start_stmt = Normalized.Pass(start_uid, exception_target) in
    let end_stmts =
      [
        Normalized.Goto(start_uid, get_next_uid ctx annot,
                        exception_target);
        Normalized.Pass(end_uid, exception_target);
      ] in
    [start_stmt] @
    test_bindings @
    test_bool_binding @
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
      normalize_expr ctx exception_target test in
    let normalized_body =
      map_and_concat (normalize_stmt ctx exception_target) body in
    let normalized_orelse =
      map_and_concat (normalize_stmt ctx exception_target) orelse in

    let test_bool_binding, test_bool_name =
      gen_normalized_assignment ctx exception_target annot
        (Normalized.Call(
            Normalized.Literal(
              Normalized.Builtin(Normalized.Builtin_bool,
                                 get_next_uid ctx annot,
                                 exception_target),
              get_next_uid ctx annot,
              exception_target),
            [update_uid ctx annot test_name],
            get_next_uid ctx annot,
            exception_target))
    in
    let end_uid = get_next_uid ctx annot in
    let end_label = Normalized.Pass(end_uid, exception_target) in
    let goto_end_label =
      Normalized.Goto(end_uid, get_next_uid ctx annot, exception_target) in
    let orelse_uid = get_next_uid ctx annot in
    let orelse_label = Normalized.Pass(orelse_uid, exception_target) in

    test_bindings @
    test_bool_binding @
    [Normalized.GotoIfNot(
        update_uid ctx annot test_bool_name,
        orelse_uid,
        get_next_uid ctx annot,
        exception_target)] @
    normalized_body @
    [
      goto_end_label;
      orelse_label
    ] @
    normalized_orelse @
    [end_label]

  | Simplified.Raise (value, annot) ->
    let value_binding, value_result = normalize_expr ctx exception_target value in
    value_binding @
    [Normalized.Raise(update_uid ctx annot value_result,
                      get_next_uid ctx annot,
                      exception_target)]

  | Simplified.TryExcept (body, handlers, annot) ->
    let handler_start_uid = get_next_uid ctx annot in
    let handler_end_uid = get_next_uid ctx annot in
    let normalized_body =
      map_and_concat (normalize_stmt ctx (Some(handler_start_uid))) body in
    let exception_name = gen_unique_name annot in
    let catch = Normalized.Catch(exception_name,
                                 handler_start_uid,
                                 exception_target) in
    let rec handlers_to_simplified_if handler_list =
      match handler_list with
      | [] -> (* If we run out of handlers, re-raise the current exception *)
        Simplified.Raise(
          Simplified.Name(exception_name, annot), annot)

      | Simplified.ExceptHandler(typ, name, body, annot)::rest ->
        let if_test = (* Check if the exception matches this type *)
          match typ with
          | None ->
            Simplified.Bool(true, annot)
          | Some(exp) ->
            Simplified.Compare(
              Simplified.Call(
                Simplified.Name("type", annot),
                [Simplified.Name(exception_name, annot)],
                annot),
              [Simplified.Eq],
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
    in (* End handler_to_if definition *)
    let normalized_handlers =
      (normalize_stmt ctx exception_target)
        (handlers_to_simplified_if handlers)
    in
    let handler_body =
      [catch] @
      normalized_handlers @
      [Normalized.Pass(handler_end_uid, exception_target)]
    in
    normalized_body @
    [Normalized.Goto(handler_end_uid,
                     get_next_uid ctx annot,
                     exception_target)] @
    handler_body

  | Simplified.Pass (annot) ->
    [Normalized.Pass (get_next_uid ctx annot,
                      exception_target)]

  | Simplified.Break (annot) ->
    begin
      match loop_end_uid with
      | None -> failwith "'break' outside loop"
      | Some(u) -> [Normalized.Goto(u, get_next_uid ctx annot,
                                    exception_target)]
    end

  | Simplified.Continue (annot) ->
    begin
      match loop_start_uid with
      | None -> failwith "'continue' not properly in loop"
      | Some(u) -> [Normalized.Goto(u, get_next_uid ctx annot,
                                    exception_target)]
    end

  | Simplified.Expr (e, annot) ->
    let bindings, result = normalize_expr ctx exception_target e in
    bindings @ [Normalized.SimpleExprStmt(update_uid ctx annot result,
                                          get_next_uid ctx annot,
                                          exception_target)]

(* Given a simplified expr, returns a list of statements, corresponding to
   the assignments necessary to compute it, and the name of the final
   variable that was bound *)
and normalize_expr
    ctx
    exception_target
    (e : 'a Simplified.expr)
  : Normalized.stmt list * Normalized.simple_expr =
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
      | hd::[] -> normalize_expr ctx exception_target hd
      | hd::tl ->
        let test_bindings, test_result =
          (normalize_expr ctx exception_target hd) in
        let tmp_name = gen_unique_name annot in
        let tmp_binding =
          Normalized.Assign(tmp_name,
                            Normalized.SimpleExpr(
                              update_uid ctx annot test_result,
                              get_next_uid ctx annot,
                              exception_target),
                            get_next_uid ctx annot,
                            exception_target)
        in
        let if_exp =
          begin
            match op with
            | Simplified.And ->
              Simplified.IfExp(
                Simplified.Name(tmp_name, annot),
                Simplified.BoolOp (op, tl, annot),
                Simplified.Name(tmp_name, annot),
                annot)
            | Simplified.Or ->
              Simplified.IfExp(
                Simplified.Name(tmp_name, annot),
                Simplified.Name(tmp_name, annot),
                Simplified.BoolOp (op, tl, annot),
                annot)
          end
        in
        let bindings, result =
          normalize_expr ctx exception_target if_exp
        in
        test_bindings @ [tmp_binding] @ bindings,
        result

    end

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
    let test_bindings, test_result = normalize_expr ctx exception_target test in
    let test_bool_binding, test_bool_name =
      gen_normalized_assignment ctx exception_target annot
        (Normalized.Call(
            Normalized.Literal(
              Normalized.Builtin(Normalized.Builtin_bool,
                                 get_next_uid ctx annot,
                                 exception_target),
              get_next_uid ctx annot,
              exception_target),
            [update_uid ctx annot test_result],
            get_next_uid ctx annot,
            exception_target))
    in
    let body_bindings, body_result = normalize_expr ctx exception_target body in
    let body_bindings_full =
      body_bindings @ [
        Normalized.Assign(tmp_name,
                          Normalized.SimpleExpr(
                            update_uid ctx annot body_result,
                            get_next_uid ctx annot,
                            exception_target),
                          get_next_uid ctx annot,
                          exception_target)
      ] in
    let orelse_bindings, orelse_result = normalize_expr ctx exception_target orelse in
    let orelse_bindings_full =
      orelse_bindings @ [
        Normalized.Assign(tmp_name,
                          Normalized.SimpleExpr(
                            update_uid ctx annot orelse_result,
                            get_next_uid ctx annot,
                            exception_target),
                          get_next_uid ctx annot,
                          exception_target)
      ] in
    let end_uid = get_next_uid ctx annot in
    let end_stmt = Normalized.Pass(end_uid, exception_target) in
    let goto_end_stmt =
      Normalized.Goto(end_uid, get_next_uid ctx annot, exception_target) in
    let orelse_uid = get_next_uid ctx annot in
    let orelse_stmt = Normalized.Pass(end_uid, exception_target) in

    let run_test =
      Normalized.GotoIfNot(
        update_uid ctx annot test_bool_name,
        orelse_uid,
        get_next_uid ctx annot,
        exception_target)
    in

    test_bindings @
    test_bool_binding @
    [run_test] @
    body_bindings_full @
    [
      goto_end_stmt;
      orelse_stmt;
    ] @
    orelse_bindings_full @
    [end_stmt],
    Normalized.Name(tmp_name, get_next_uid ctx annot,
                    exception_target)

  | Simplified.Compare (left, ops, comparators, annot) ->
    (* "x < y < z" is equivalent to "x < y and y < z", except y is only
       evaluated once. We treat compare in almost exactly the same way
       as we treat boolean operators.

       Specifically, we turn "x < y < z < ..."

       into

       tmp1 = x
       tmp2 = y
       tmp3 = tmp1.__lt__(y)
       if tmp3 then (tmp2 < z < ...) else tmp3*)
    let left_bindings, left_result = normalize_expr ctx exception_target left in
    begin
      match ops with
      | [] -> failwith "No operation given to comparison"
      | hd::tl ->
        let right_bindings, right_result =
          normalize_expr ctx exception_target (List.hd comparators) in
        let cmp_func_bindings, cmp_func_result =
          gen_normalized_assignment ctx exception_target annot
            (Normalized.Attribute(left_result,
                                  normalize_cmpop hd,
                                  get_next_uid ctx annot,
                                  exception_target))
        in
        let cmp_bindings, cmp_result =
          gen_normalized_assignment ctx exception_target annot
            (Normalized.Call(cmp_func_result,
                             [right_result],
                             get_next_uid ctx annot,
                             exception_target))
        in
        let all_bindings = left_bindings @
                           right_bindings @
                           cmp_func_bindings @
                           cmp_bindings
        in
        begin
          match tl with
          | [] -> all_bindings, cmp_result
          | _ ->
            let if_exp =
              Simplified.IfExp(
                Simplified.Name(id_of_name cmp_result, annot),
                Simplified.Compare(Simplified.Name(id_of_name right_result, annot),
                                   tl,
                                   List.tl comparators,
                                   annot),
                Simplified.Name(id_of_name cmp_result, annot),
                annot)
            in
            let bindings, result = normalize_expr ctx exception_target if_exp in
            all_bindings @ bindings, result
        end
    end

  | Simplified.Call (func, args, annot) ->
    let func_bindings, func_name = normalize_expr ctx exception_target func in
    let arg_bindings, arg_names = normalize_expr_list ctx exception_target args in
    let assignment, name = gen_normalized_assignment ctx exception_target annot
        (Normalized.Call(update_uid ctx annot func_name,
                         List.map (update_uid ctx annot) arg_names,
                         get_next_uid ctx annot,
                         exception_target)) in
    let bindings = func_bindings @ arg_bindings @ assignment in
    bindings, name

  | Simplified.Num (n, annot) ->
    ([], Normalized.Literal(
        Normalized.Num(normalize_number n, get_next_uid ctx annot,
                       exception_target),
        get_next_uid ctx annot,
        exception_target))

  | Simplified.Str (s, annot) ->
    ([], Normalized.Literal(
        Normalized.Str(normalize_str s, get_next_uid ctx annot,
                       exception_target),
        get_next_uid ctx annot,
        exception_target))

  | Simplified.Bool (b, annot) ->
    ([], Normalized.Literal(
        Normalized.Bool(b, get_next_uid ctx annot,
                        exception_target),
        get_next_uid ctx annot,
        exception_target))

  | Simplified.Builtin (b, annot) ->
    ([], Normalized.Literal(
        Normalized.Builtin(normalize_builtin b, get_next_uid ctx annot,
                        exception_target),
        get_next_uid ctx annot,
        exception_target))

  | Simplified.Attribute (obj, attr, annot) ->
    let obj_bindings, obj_result = normalize_expr ctx exception_target obj in
    let assignment, result = gen_normalized_assignment ctx exception_target annot
        (Normalized.Attribute(update_uid ctx annot obj_result,
                              attr,
                              get_next_uid ctx annot,
                              exception_target)) in
    obj_bindings @ assignment, result

  | Simplified.Name (id, annot) ->
    ([], Normalized.Name(id, get_next_uid ctx annot,
                         exception_target))

  | Simplified.List (elts, annot) ->
    let bindings, results = normalize_expr_list ctx exception_target elts in
    let assignment, name =
      gen_normalized_assignment ctx exception_target annot
        (Normalized.List(List.map (update_uid ctx annot) results,
                         get_next_uid ctx annot,
                         exception_target)) in
    bindings @ assignment, name

  | Simplified.Tuple (elts, annot) ->
    let bindings, results = normalize_expr_list ctx exception_target elts in
    let assignment, name =
      gen_normalized_assignment ctx exception_target annot
        (Normalized.Tuple(List.map (update_uid ctx annot) results,
                          get_next_uid ctx annot,
                          exception_target)) in
    bindings @ assignment, name

(* Given a list of exprs, returns a list containing all of their
   bindings and a list containing all of the relevant variable names *)
and normalize_expr_list ctx exception_target (lst : 'a Simplified.expr list) =
  normalize_list (normalize_expr ctx exception_target) lst

and normalize_expr_option ctx exception_target
    (o : 'a Simplified.expr option)
  : Normalized.stmt list * Normalized.simple_expr option =
  let normalized_opt =
    normalize_option (normalize_expr ctx exception_target) o in
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
  | Simplified.NotIn -> failwith "the NotIn operator is not supported" (* TODO *)

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

and normalize_builtin b =
  match b with
  | Simplified.Builtin_bool -> Normalized.Builtin_bool
| Simplified.Builtin_slice -> Normalized.Builtin_slice
