open Batteries;;
(* open Jhupllib;; *)
open Python2_ast_types;;
module Augmented = Python2_augmented_ast;;
module Simplified = Python2_simplified_ast;;
open Python2_simplification_utils;;

open Python_simplification_monad;;
open Simplification_monad;;

let map_and_concat (func : 'a -> 'b list) (lst : 'a list) =
  List.concat (List.map func lst)
;;

let rec simplify_modl (ctx : Unique_name_ctx.name_context) (m : Augmented.annotated_modl)
  : Simplified.annotated_modl =
  let Augmented.Module(body) = m.body in
  let _, simplified_body = run ctx m.annot @@ simplify_stmt_list body in
  annotate m.annot @@
  Simplified.Module(simplified_body)

and simplify_stmt_list (stmts : Augmented.annotated_stmt list) : unit m =
  let%bind _ = sequence @@ List.map simplify_stmt stmts in
  return ()

and simplify_stmt (s : Augmented.annotated_stmt) : unit m =
  local_annot s.annot @@
  let annotate body = annotate s.annot body in
  match s.body with
  | Augmented.FunctionDef (func_name, args, body)->
    let body = add_return body s.annot in
    let%bind _, simplified_body = listen @@ simplify_stmt_list body in
    emit
      [Simplified.Assign(func_name,
                         annotate @@ Simplified.FunctionVal(
                           args,
                           simplified_body))]
  | Augmented.Return (value) ->
    begin
      match value with
      | None ->
        let%bind value_name =
          gen_assignment @@ annotate @@
          Simplified.Builtin(Builtin_None)
        in
        emit
          [
            Simplified.Return(value_name)
          ]
      | Some(e) ->
        let%bind id = simplify_expr e in
        emit
          [Simplified.Return(id)]
    end

  | Augmented.Assign (targets, value) ->
    (* Assignments are very complicated, with different behavior depending
       on the form of the target.

       targets is a list, which will have multiple entries if we used
       the syntax x = y = ... = 2.

       value is the expression we are assigning from. This is only ever
       evaluated once, no matter what we're assigning to. *)

    let%bind value_id = simplify_expr value in
    let value_name = annotate @@ Simplified.Name(value_id) in

    let simplify_assignment (target : Augmented.annotated_expr) : unit m =
      match target.body with
      | Augmented.Name (id) ->
        emit [Simplified.Assign(id, value_name)]

      | Augmented.Attribute (obj, attr) ->
        let%bind obj_result = simplify_expr obj in
        let%bind attr_name = gen_assignment @@ annotate @@
          Simplified.Str(attr) (* Implictly cast identifer to string *)
        in
        let%bind attr_result = gen_assignment @@ annotate @@
          Simplified.Attribute(
            obj_result,
            "__setattr__")
        in
        let%bind _ = gen_assignment @@ annotate @@
          Simplified.Call(attr_result, [attr_name; value_id])
        in
        return ()

      | Augmented.Subscript (lst, slice) ->
        let%bind lst_result = simplify_expr lst in
        let%bind slice_result = simplify_slice slice in
        let%bind attr_result = gen_assignment @@ annotate @@
          Simplified.Attribute(
            lst_result,
            "__setitem__")
        in
        let%bind _ = gen_assignment @@ annotate @@
          Simplified.Call(attr_result, [slice_result; value_id])
        in
        return ()

      (* To assign to a list or tuple, we iterate through value
         (which must therefore be iterable), and assign the elements
         of our lhs in order. If the number of elements doesn't match,
         we raise a ValueError and do not assign any of the values.

         We turn "i,j,k = list" into

         iter_val = list.__iter__()
         try:
           tmp_i = iter_val.__next__()
           tmp_j = iter_val.__next__()
           tmp_k = iter_val.__next__()

           try: # Make sure there aren't too many variables
             iter_val().__next__ # Should raise a StopIteration exception
             raise ValueError, "too many elements to unpack"
           except StopIteration:
             pass

         except StopIteration:
           raise ValueError, "Too few elements to unpack"

         # We have the right number of elements, so start assigning
         # This potentially involves recursion
         i = tmp_i
         j = tmp_j
         k = tmp_k
      *)
      | Augmented.List targets
      | Augmented.Tuple targets ->
        let%bind iter_val = fresh_name () in
        let%bind _ = (* iter_val = seq.__iter__().next *)
          simplify_stmt @@
          annotate @@ Augmented.Assign(
            [annotate @@ Augmented.Name(iter_val)],
            annotate @@ Augmented.Call(
              annotate @@ Augmented.Attribute(
                annotate @@ Augmented.Name(value_id),
                "__iter__"),
              []))
        in
        (* list of the names tmp_i, tmp_j, etc *)
        let%bind tmp_names =
          sequence @@ List.map (fun _ -> fresh_name ()) targets
        in
        let tmp_assignments =
          List.map
            (fun tmp_name ->
               annotate @@ Augmented.Assign( (* tmp_i = iter_val() *)
                 [annotate @@ Augmented.Name(tmp_name)],
                 annotate @@ Augmented.Call(
                   annotate @@ Augmented.Attribute(
                     annotate @@ Augmented.Name(iter_val),
                     "__next__"),
                   []))
            )
            tmp_names
        in

        let subordinate_try_except =
          annotate @@ Augmented.TryExcept(
            [
              (* Should raise StopIteration *)
              annotate @@ Augmented.Expr(
                annotate @@ Augmented.Call(
                  annotate @@ Augmented.Attribute(
                    annotate @@ Augmented.Name(iter_val),
                    "__next__"),
                  []))
              ;
              annotate @@ Augmented.Raise(
                Some(
                  annotate @@ Augmented.Call(
                    annotate @@ Augmented.Builtin(Builtin_ValueError),
                    [annotate @@ Augmented.Str("too many values to unpack")]
                  ))
              )
              ;
            ],
            [
              Augmented.ExceptHandler(
                Some(annotate @@ Augmented.Builtin(Builtin_StopIteration)),
                None,
                [ annotate @@ Augmented.Pass ])
            ],
            [])
        in

        let overall_try_except =
          annotate @@ Augmented.TryExcept(
            tmp_assignments @ [subordinate_try_except],
            [
              Augmented.ExceptHandler(
                Some(annotate @@ Augmented.Builtin(Builtin_StopIteration)),
                None,
                [
                  annotate @@ Augmented.Raise(
                    Some(annotate @@ Augmented.Call(
                        annotate @@ Augmented.Builtin(Builtin_ValueError),
                        [annotate @@ Augmented.Str(
                            (* TODO: In Python this has the actual number of
                               elts that it successfully unpacked *)
                            "too few values to unpack")])));
                ])
            ],
            [])
        in
        let%bind _ = simplify_stmt overall_try_except in
        (* Now we just have to assign the values. Since the values
           might still be complex (e.g. tuples), we have to do this
           recursively. *)
        let recursive_assignments =
          List.map2
            (fun
              (target : Augmented.annotated_expr)
              (tmp_name : identifier) ->
              annotate @@ Augmented.Assign(
                [target],
                annotate @@ Augmented.Name(tmp_name))
            )
            targets tmp_names
        in
        simplify_stmt_list recursive_assignments

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
        (* End of simplify_assignment *)
    in

    let%bind _ = sequence @@ List.map simplify_assignment targets in
    empty

  | Augmented.AugAssign (target, op, value) ->
    let%bind simplified_value = simplify_expr value in
    let%bind result = fresh_name () in (* x0 *)

    (* Depending on the form of target, we will do different things during setup
       and at the end. *)
    let%bind (simplified_target : identifier),
             (final_assignment : Augmented.annotated_stmt) =
      get_setup_and_final_assignment_for_augmented_assignment
        target result simplify_expr simplify_slice
    in

    let%bind result = fresh_name () in (* x0 *)
    let gen_try = gen_augmented_try_for_binop result in

    (* Look for e.g. __iadd__ *)
    let%bind first_try =
      gen_try simplified_target simplified_value (simplify_ioperator op)
    in
    let%bind () = simplify_stmt first_try in

    (* Look for e.g. __add__ *)
    let%bind is_notimplemented = check_if_notimplemented result in
    let%bind second_try =
      gen_try simplified_target simplified_value (simplify_operator op)
    in
    let run_second_try = annotate @@
      Augmented.If(
        is_notimplemented,
        [second_try],
        []
      )
    in
    let%bind () = simplify_stmt run_second_try in

    (* Look for e.g. __radd__ *)
    let%bind is_notimplemented = check_if_notimplemented result in
    let%bind is_same_type = check_if_same_type simplified_target simplified_value in
    let%bind third_try =
      gen_try simplified_value simplified_target (simplify_roperator op)
    in

    let run_third_try = annotate @@
      Augmented.If(
        annotate @@ Augmented.BoolOp(Augmented.And, [is_notimplemented; is_same_type]),
        [third_try],
        []
      )
    in
    let%bind () = simplify_stmt run_third_try in

    (* Raise error if we haven't found anything *)
    let%bind is_notimplemented = check_if_notimplemented result in
    let raise_exn = annotate @@
      Augmented.Raise(Some(
          annotate @@
          Augmented.Call(annotate @@ Augmented.Builtin(Builtin_TypeError),
                         [annotate @@ Augmented.Str("Unsupported operands for binop")])))
    in
    simplify_stmt @@ annotate @@
      Augmented.If(is_notimplemented,
                   [raise_exn],
                   [final_assignment])

  | Augmented.For (target, seq, body, orelse) ->
    (* For loops are always over some iterable. According to the docs,
       they loop until they are told to stop, or their iterator stops
       returning objects.

       The targets are assigned to at the beginning of the body, then the
       body's code is executed. When there are no elements of the iterable
       to assign, the loop terminates.

       We handle for loops by turning them into a concrete while loop,
       then simplifying the resulting code. Specifically, we turn

       for i in seq:
         <body>
       else:
         <else>

       into

       iter_val = seq.__iter__()
       try:
         while True:
           i = next_val.__next__()
           <body>
       except StopIteration:
         <else>

    *)
    let%bind iter_val = fresh_name () in
    let bind_iter_val = (* iter_val = seq.__iter__() *)
      annotate @@
      Augmented.Assign(
        [annotate @@ Augmented.Name(iter_val)],
        annotate @@ Augmented.Call(
          annotate @@ Augmented.Attribute(
            seq,
            "__iter__"
          ),
          []))
    in
    let assign_target = (* i = iter_val.__next__() *)
      annotate @@
      Augmented.Assign(
        [target],
        annotate @@ Augmented.Call(
          annotate @@ Augmented.Attribute(
            annotate @@ Augmented.Name(iter_val),
            "__next__"
          ),
          []))
    in
    let while_loop = (* while True: i = next_val.__next__(); <body> *)
      annotate @@
      Augmented.While(
        annotate @@ Augmented.Bool(true),
        [assign_target] @ body,
        [])
    in
    let try_except = (* try: <while>; except StopIteration: <else> *)
      annotate @@
      Augmented.TryExcept(
        [while_loop],
        [Augmented.ExceptHandler(
            Some(annotate @@ Augmented.Builtin(Builtin_StopIteration)),
            None,
            orelse
          )],
        [])
    in
    simplify_stmt_list [bind_iter_val; try_except]

  | Augmented.While (test, body, orelse) ->
    let%bind test_result, test_bindings = listen @@ simplify_expr test in
    let%bind test_name = fresh_name () in
    let%bind _, assignment = listen @@
      let%bind bool_name = gen_assignment @@ annotate @@ Simplified.Builtin(Builtin_bool) in
      emit
        [
          Simplified.Assign(test_name,
                            annotate @@ Simplified.Call(bool_name, [test_result]))
        ]
    in
    let full_test_bindings = test_bindings @ assignment in

    let%bind _, simplified_body = listen @@ simplify_stmt_list body in
    let%bind _, simplified_orelse = listen @@ simplify_stmt_list orelse in
    let%bind _ = emit_stmts full_test_bindings in
    emit @@
    [
      Simplified.While(test_name, simplified_body @ full_test_bindings, simplified_orelse)
    ]

  | Augmented.If (test, body, orelse) ->
    let%bind test_result = simplify_expr test in
    let%bind _, simplified_body = listen @@ simplify_stmt_list body in
    let%bind _, simplified_orelse = listen @@ simplify_stmt_list orelse in
    let%bind test_name = fresh_name () in
    let%bind bool_name = gen_assignment @@ annotate @@ Simplified.Builtin(Builtin_bool) in
    emit
      [
        Simplified.Assign(test_name,
                          annotate @@ Simplified.Call(bool_name, [test_result]));

        Simplified.If(
          test_name,
          simplified_body,
          simplified_orelse)
      ]

  | Augmented.Raise (typ) ->
    let%bind typ_result =
      match typ with
      | None -> failwith "Raise must have exactly one argument"
      | Some(e) -> simplify_expr e
    in
    emit
      [
        Simplified.Raise(typ_result)
      ]

  | Augmented.TryExcept (body, handlers, orelse) ->
    let%bind _, simplified_body = listen @@ simplify_stmt_list body in
    let%bind exn_id = fresh_name () in
    let%bind _, simplified_handlers = listen @@ simplify_excepthandlers exn_id handlers in
    let%bind _, simplified_orelse = listen @@ simplify_stmt_list orelse in
    emit
      [
        Simplified.TryExcept(simplified_body, exn_id, simplified_handlers, simplified_orelse)
      ]


  | Augmented.Expr (e) ->
    let%bind _ = simplify_expr e in
    return ()

  | Augmented.Pass ->
    emit [Simplified.Pass]

  | Augmented.Break ->
    emit [Simplified.Break]

  | Augmented.Continue ->
    emit [Simplified.Continue]

(* Given a concrete expr, returns a list of statements, corresponding to
   the assignments necessary to compute it, and the name of the final
   variable that was bound *)
and simplify_expr (e : Augmented.annotated_expr) : identifier m =
  local_annot e.annot @@
  let annotate body = annotate e.annot body in
  match e.body with

  (* BoolOps are a tricky case because of short-circuiting. We need to
     make sure that when we evaluate "a and False and b", b is never
     evaluated, etc.

     We do this by iteratively breaking down the statements like so:
     "a and b and c and ..." turns into

     "if a then (b and c and ...) else a", except we make sure that
     a is only evaluated once by storing a in a tmp variable after it's
     evaluated.

     To avoid duplicate code, we construct a concrete IfExp to represent
     the above code, then recursively simplify that.
  *)
  | Augmented.BoolOp (op, operands) ->
    begin
      match operands with
      | [] -> failwith "No arguments to BoolOp"
      | hd::[] -> simplify_expr hd
      | hd::tl ->
        let%bind test_result = simplify_expr hd in
        let if_exp = annotate @@
          begin
            match op with
            | Augmented.And ->
              Augmented.IfExp(
                annotate @@ Augmented.Name(test_result),
                annotate @@ Augmented.BoolOp (op, tl),
                annotate @@ Augmented.Name(test_result))
            | Augmented.Or ->
              Augmented.IfExp(
                annotate @@ Augmented.Name(test_result),
                annotate @@ Augmented.Name(test_result),
                annotate @@ Augmented.BoolOp (op, tl))
          end
        in
        simplify_expr if_exp

    end

  | Augmented.BinOp (left, op, right) ->
    let%bind left_result = simplify_expr left in
    let%bind right_result = simplify_expr right in

    let%bind result = fresh_name () in (* x0 *)
    let gen_try = gen_augmented_try_for_binop result in

    (* Look for e.g. __add__ *)
    let%bind first_try =
      gen_try left_result right_result (simplify_operator op)
    in
    let%bind () = simplify_stmt first_try in

    (* Look for e.g. __radd__ *)
    let%bind second_try =
      gen_try right_result left_result (simplify_roperator op)
    in
    let%bind is_notimplemented = check_if_notimplemented result in
    let%bind is_same_type = check_if_same_type left_result right_result in

    let run_second_try = annotate @@
      Augmented.If(
        annotate @@ Augmented.BoolOp(Augmented.And, [is_notimplemented; is_same_type]),
        [second_try],
        []
      )
    in
    let%bind () = simplify_stmt run_second_try in

    (* Raise error if we haven't found anything *)
    let%bind is_notimplemented = check_if_notimplemented result in
    let raise_exn = annotate @@
      Augmented.Raise(Some(
          annotate @@
          Augmented.Call(annotate @@ Augmented.Builtin(Builtin_TypeError),
                         [annotate @@ Augmented.Str("Unsupported operands for binop")])))
    in
    let%bind () = simplify_stmt @@ annotate @@
      Augmented.If(is_notimplemented,
                   [raise_exn],
                   [])
    in

    (* That's it! Return the variable which will hold the result if it exists *)
    return result

  | Augmented.UnaryOp (op, operand) ->
    let%bind op_result = simplify_expr operand in
    begin
      match op with
      | Augmented.Not ->
        let%bind bool_name = simplify_expr @@
          annotate @@ Augmented.Call(annotate @@ Augmented.Builtin(Builtin_bool),
                                     [annotate @@ Augmented.Name(op_result)])
        in
        gen_assignment @@ annotate @@
        Simplified.UnaryOp(Simplified.Not, bool_name)

      | Augmented.UAdd ->
        let%bind attr_name = gen_assignment @@ annotate @@
          Simplified.Attribute(op_result, "__pos__")
        in
        gen_assignment @@ annotate @@
        Simplified.Call(attr_name, [])

      | Augmented.USub ->
        let%bind attr_name = gen_assignment @@ annotate @@
          Simplified.Attribute(op_result, "__neg__")
        in
        gen_assignment @@ annotate @@
        Simplified.Call(attr_name, [])
    end

  | Augmented.IfExp (test, body, orelse) ->
    (* Python allows expressions like 'x = 1 if test else 0'. Of course,
       only the relevant branch is executed, so we can't simply evaluate both
       beforehand. But in order to be on the right-hand-side of an assignment,
       the expression must be no more complicated than a compound_expr. In
       particular, the expression can't be an assignment.
       simplification
       So to evaluate "x = y if test else z", we first create an if _statement_
       and then use the results of that.
       if test:
         # evaluate and bind tmp = y
       else:
         # evaluate and bind tmp = z
       x = tmp *)

    let%bind test_result = simplify_expr test in
    let%bind body_result, body_bindings = listen @@ simplify_expr body in
    let%bind orelse_result, orelse_bindings = listen @@ simplify_expr orelse in
    let%bind result_name = fresh_name () in
    let%bind bool_name = gen_assignment @@ annotate @@ Simplified.Builtin(Builtin_bool) in
    let%bind test_name = gen_assignment @@ annotate @@
      Simplified.Call(bool_name, [test_result])
    in
    let%bind _ =  emit
        [
          Simplified.If(test_name,
                        body_bindings @
                        [annotate @@ Simplified.Assign(result_name,
                                                       annotate @@ Simplified.Name(body_result))],
                        orelse_bindings @
                        [annotate @@ Simplified.Assign(result_name,
                                                       annotate @@ Simplified.Name(orelse_result))])
        ]
    in
    return result_name
      (*
  | Augmented.Compare (left, ops, comparators, annot) ->
    (* "x < y < z" is equivalent to "x < y and y < z", except y is only
       evaluated once. We treat compare in almost exactly the same way
       as we treat boolean operators.

       Specifically, we turn "x < y < z < ..."

       into

       tmp1 = x
       tmp2 = y
       tmp3 = tmp1.__lt__
       tmp4 = tmp3(tmp2)
       if tmp4 then (tmp2 < z < ...) else tmp4
    *)
    begin
      match ops with
      | [] -> failwith "No right operand given to compare"
      | op::rest ->
        let left_bindings, left_result = simplify_expr left in
        let right_bindings, right_result = simplify_expr (List.hd comparators) in
        begin
          match rest with
          | [] ->
            (* Don't generate a temporary variable if this is the last one *)
            left_bindings @ right_bindings,
            generate_comparison left_result op right_result annot
          | _ ->
            let right_id = gen_unique_name annot in
            let assign = Simplified.Assign(right_id, right_result, annot) in
            let bindings = left_bindings @ right_bindings @ [assign] in
            let comparison =
              generate_comparison left_result op (Simplified.Name(right_id, annot)) annot
            in

            let comparison_id = gen_unique_name annot in
            let cmp_assign = Simplified.Assign(comparison_id, comparison, annot) in
            let comparison_name = Augmented.Name(comparison_id, annot) in
            let rest_of_bindings, rest_of_comparison =
              simplify_expr @@
              Augmented.IfExp(comparison_name,
                              Augmented.Compare(Augmented.Name(right_id, annot),
                                                rest, List.tl comparators, annot),
                              comparison_name,
                              annot)
            in
            bindings @ [cmp_assign] @ rest_of_bindings, rest_of_comparison
        end
    end
*)
  | Augmented.Call (func, args) ->
    let%bind func_result = simplify_expr func in
    let%bind arg_results = simplify_list simplify_expr args in
    gen_assignment @@ annotate @@
    Simplified.Call (func_result, arg_results)

  | Augmented.Num (n) ->
    gen_assignment @@ annotate @@
    Simplified.Num(n)

  | Augmented.Str (s) ->
    gen_assignment @@ annotate @@
    Simplified.Str(s)

  | Augmented.Bool (b) ->
    gen_assignment @@ annotate @@
    Simplified.Bool(b)

  | Augmented.Attribute (obj, attr) ->
    let%bind obj_result = simplify_expr obj in
    gen_assignment @@ annotate @@
    Simplified.Attribute (obj_result, attr)

  (* Turn subscripts into calls to __getitem__() *)
  | Augmented.Subscript (value, slice) ->
    let%bind value_result = simplify_expr value in
    let%bind slice_result = simplify_slice slice in
    let%bind attr_name =
      gen_assignment @@ annotate @@
      Simplified.Attribute(
        value_result,
        "__getitem__")
    in
    gen_assignment @@ annotate @@
    Simplified.Call(attr_name, [slice_result])

  | Augmented.Name (id) -> return id

  | Augmented.List (elts) ->
    let%bind elt_ids = sequence @@ List.map simplify_expr elts in
    gen_assignment @@ annotate @@
    Simplified.List(elt_ids)

  | Augmented.Tuple (elts) ->
    let%bind elt_ids = sequence @@ List.map simplify_expr elts in
    gen_assignment @@ annotate @@
    Simplified.Tuple(elt_ids)

  | Augmented.NoneExpr ->
    gen_assignment @@ annotate @@
    Simplified.Builtin(Builtin_None)

  | Augmented.Builtin (b) ->
    gen_assignment @@ annotate @@
    Simplified.Builtin(b)

  | _ -> failwith "NYI"

(* Turn a slice operator into a call to the slice() function, with
   the same arguments. E.g. 1:2:3 becomes slice(1,2,3), and
   1:2 becomes slice (1,2,None) *)
and simplify_slice  (s : Augmented.slice) : identifier m =
  ignore s; failwith "NYI"
(* Turn a "None" option into the python "None" object, and turn a
   "Some" option into the simplified version of its contents *)
  (*
  let exp_opt_to_slice_arg e =
    match e with
    | None ->
      [], Simplified.Name("*None", annot)
    | Some(x) ->
      simplify_expr ctx x
  in
  match s with
  | Augmented.Slice (lower, upper, step) ->
    let lower_bindings, lower_result = exp_opt_to_slice_arg lower in
    let upper_bindings, upper_result = exp_opt_to_slice_arg upper in
    let step_bindings, step_result = exp_opt_to_slice_arg step in
    let args_list =
      [
        lower_result;
        upper_result;
        step_result;
      ] in
    lower_bindings @ upper_bindings @ step_bindings,
    Simplified.Call(
      Simplified.Builtin(Builtin_slice, annot),
      args_list,
      annot
    )
  | Augmented.Index (value) ->
    simplify_expr ctx value
*)

and simplify_operator o =
  match o with
  | Augmented.Add  ->  "__add__"
  | Augmented.Sub  ->  "__sub__"
  | Augmented.Mult ->  "__mul__"
  | Augmented.Div  ->  "__div__"
  | Augmented.Mod  ->  "__mod__"
  | Augmented.Pow  ->  "__pow__"

and simplify_ioperator o =
  match o with
  | Augmented.Add  ->  "__iadd__"
  | Augmented.Sub  ->  "__isub__"
  | Augmented.Mult ->  "__imul__"
  | Augmented.Div  ->  "__idiv__"
  | Augmented.Mod  ->  "__imod__"
  | Augmented.Pow  ->  "__ipow__"

and simplify_roperator o =
  match o with
  | Augmented.Add  ->  "__radd__"
  | Augmented.Sub  ->  "__rsub__"
  | Augmented.Mult ->  "__rmul__"
  | Augmented.Div  ->  "__rdiv__"
  | Augmented.Mod  ->  "__rmod__"
  | Augmented.Pow  ->  "__rpow__"

and simplify_cmpop o =
  match o with
  | Augmented.Eq -> "__eq__"
  | Augmented.NotEq -> "__ne__"
  | Augmented.Lt -> "__lt__"
  | Augmented.LtE -> "__le__"
  | Augmented.Gt -> "__gt__"
  | Augmented.GtE -> "__ge__"
  | Augmented.In -> "__contains__"
  | Augmented.NotIn
  | Augmented.Is
  | Augmented.IsNot -> failwith "No corresponding comparison function"

and simplify_excepthandlers exn_id handlers : unit m =
  ignore exn_id; ignore handlers; failwith "NYI"
    (*
  match handlers with
  | [] ->
    [Simplified.Raise(Simplified.Name(exn_id, annot), annot)]

  | Augmented.ExceptHandler (typ, name, body, annot)::rest ->
    let typ_bindings, typ_result =
      match typ with
      | None -> [], Simplified.Bool(true, annot)
      | Some(t) ->
        simplify_expr ctx @@
        Augmented.Compare(
          (* We use exn.__class__ instead of type(exn) to avoid having to think
             about the type function. They behave the same since exceptions are
             always new_style. *)
          Augmented.Attribute(
            Augmented.Name(exn_id, annot),
            "__class__",
            annot),
          [Augmented.Is],
          [t],
          annot)
    in
    let name_bind =
      match name with
      | None -> []
      | Some(Augmented.Name(id,_)) ->
        [Simplified.Assign(id, Simplified.Name(exn_id, annot), annot)]
      | _ -> failwith "Second argument to exception handler must be an identifier"
    in
    typ_bindings @
    [
      Simplified.If(
        Simplified.Call(Simplified.Builtin(Builtin_bool, annot),
                        [typ_result],
                        annot),
        name_bind @ map_and_concat (simplify_stmt ctx) body,
        simplify_excepthandlers ctx exn_id rest annot,
        annot
      )]

and generate_comparison lhs op rhs annot =
  match op with
  | Augmented.Is ->
    Simplified.Binop(lhs, Simplified.Is, rhs, annot)
  | Augmented.IsNot ->
    Simplified.UnaryOp(Simplified.Not,
                       Simplified.Call(
                         Simplified.Builtin(Builtin_bool, annot),
                         [Simplified.Binop(lhs, Simplified.Is, rhs, annot)],
                         annot),
                       annot)
  | Augmented.NotIn -> raise @@ Utils.Not_yet_implemented "NotIn simplification"
  | _ ->
    (* FIXME: comparison operators are actually a lot more complicated *)
    let cmp_func = simplify_cmpop op in
    Simplified.Call(
      Simplified.Attribute(lhs, cmp_func, annot),
      [rhs],
      annot) *)
