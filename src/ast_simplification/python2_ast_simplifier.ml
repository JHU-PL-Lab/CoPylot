open Python2_ast_types
module Concrete = Python2_concrete_ast
module Simplified = Python2_simplified_ast
exception Invalid_assignment of string

(* FIXME: We need to create a type for builtin methods (in addition to builtin
   functions such as type, bool, slice) such as __getattr__, and use that
   instead of just a string whenever we invoke them in this file *)

let name_counter = ref 0;;
let use_shortened_names = ref false;;

let gen_unique_name _ =
  let count = !name_counter in
  name_counter := count + 1;
  let prefix = if !use_shortened_names
    then
      "$simp"
    else
      "$simplified_unique_name_"
  in prefix ^ string_of_int count
;;

let reset_unique_name () = name_counter := 0;;
let toggle_short_names (b : bool) = use_shortened_names := b;;

let map_and_concat (func : 'a -> 'b list) (lst : 'a list) =
  List.concat (List.map func lst)
;;

let simplify_option func o =
  match o with
  | None -> None
  | Some(x) -> Some(func x)
;;

let rec simplify_modl (m : 'a Concrete.modl) : 'a Simplified.modl =
  match m with
  | Concrete.Module (body, annot) ->
    Simplified.Module(map_and_concat simplify_stmt body, annot)

and simplify_stmt
    (s : 'a Concrete.stmt)
  : 'a Simplified.stmt list =
  match s with
  | Concrete.FunctionDef (func_name, args, body, _, annot)->
    let simplified_args = simplify_arguments args in
    let simplified_body = map_and_concat simplify_stmt body in
    [Simplified.Assign(func_name,
                       Simplified.FunctionVal(
                         simplified_args,
                         simplified_body,
                         annot),
                       annot)]

  | Concrete.Return (value, annot) ->
    begin
      match simplify_expr_option value with
      | None -> [Simplified.Return(Simplified.Name("*None", annot), annot)]
      | Some(v) -> [Simplified.Return(v, annot)]
    end

  | Concrete.Assign (targets, value, annot) ->
    (* Assignments are very complicated, with different behavior depending
       on the lvalue.

       targets is a list, which will have multiple entries if we used
       the syntax x = y = ... = 2.

       value is the expression we are assigning from. This is only ever
       evaluated once, no matter what we're assigning to. *)
    let simplified_value = simplify_expr value in
    let unique_name = gen_unique_name annot in
    let value_name = Simplified.Name(unique_name, annot) in
    let value_assignment =
      Simplified.Assign(unique_name, simplified_value, annot) in
    let simplify_assignment =
      (fun e ->
         match e with
         | Concrete.Name (id, _, _) ->
           [Simplified.Assign(id,
                              value_name,
                              annot)]

         | Concrete.Attribute (obj, id, _, _) ->
           [Simplified.Expr(
               Simplified.Call(
                 Simplified.Attribute(
                   simplify_expr obj,
                   "__setattr__",
                   annot),
                 [
                   Simplified.Str(StringLiteral(id), annot);
                   value_name;
                 ],
                 annot),
               annot)]

         | Concrete.Subscript (lst, slice, _, _) ->
           [Simplified.Expr(
               Simplified.Call(
                 Simplified.Attribute(
                   simplify_expr lst,
                   "__setitem__",
                   annot),
                 [
                   simplify_slice slice annot;
                   value_name;
                 ],
                 annot),
               annot)]

         (* To assign to a list or tuple, we iterate through value
            (which must therefore be iterable), and assign the elements
            of our lhs in order. If the number of elements doesn't match,
            we raise a ValueError and do not assign any of the values.

            We turn "i,j,k = list" into

            next_val = list.__iter__().next
            try:
              tmp_i = next_val()
              tmp_j = next_val()
              tmp_k = next_val()

              try: # Make sure there aren't too many variables
                next_val() # Should raise a StopIteration exception
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
         | Concrete.List (elts, _, _)
         | Concrete.Tuple (elts, _, _) ->
           let next_val = gen_unique_name annot in
           let bind_next_val = (* next_val = seq.__iter__().next *)
             Simplified.Assign(
               next_val,
               Simplified.Attribute(
                 Simplified.Call(
                   Simplified.Attribute(
                     value_name,
                     "__iter__",
                     annot),
                   [],
                   annot),
                 "next",
                 annot),
               annot) in
           let tmp_bindings =
             List.fold_left
               (fun
                 (prev : identifier list * 'a Simplified.stmt list)
                 (_ : 'a Concrete.expr) ->
                 let tmp_name = gen_unique_name annot in
                 let next_assignment =
                   [
                     Simplified.Assign( (* tmp_i = next_val() *)
                       tmp_name,
                       Simplified.Call(Simplified.Name(next_val, annot),
                                       [],
                                       annot),
                       annot);
                   ]
                 in
                 (fst prev) @ [tmp_name], (snd prev) @ next_assignment)
               ([], []) elts in
           let subordinate_try_except =
             Simplified.TryExcept(
               [
                 Simplified.Expr(
                   Simplified.Call(
                     Simplified.Name(next_val, annot),
                     [],
                     annot),
                   annot);

                 Simplified.Raise(
                   Simplified.Call(
                     Simplified.Builtin(Builtin_ValueError, annot),
                     [Simplified.Str(
                         StringLiteral("too many values to unpack"),
                         annot)],
                     annot),
                   annot);
               ],
               [Simplified.ExceptHandler(
                   Some(Simplified.Name("StopIteration", annot)),
                   None,
                   [ Simplified.Pass(annot) ],
                   annot)],
               annot) in
           let overall_try_except =
             Simplified.TryExcept(
               (snd tmp_bindings) @ [ subordinate_try_except ],
               [Simplified.ExceptHandler(
                   Some(Simplified.Name("StopIteration", annot)),
                   None,
                   [
                     Simplified.Raise(
                       Simplified.Call(
                         Simplified.Builtin(Builtin_ValueError, annot),
                         [Simplified.Str(
                             (* TODO: In Python this has the actual number of
                                elts that it successfully unpacked *)
                             StringLiteral("too few values to unpack"),
                             annot)],
                         annot),
                       annot);
                   ],
                   annot)],
               annot) in
           let verification =
             [
               bind_next_val;
               overall_try_except;
             ] in
           (* Now we just have to assign the values. Since the values
              might still be complex (e.g. tuples), we have to do this
              recursively. *)
           let assignment_list =
             List.map2
               (fun
                 (tuple_elt : 'a Concrete.expr)
                 (tmp_name : identifier) ->
                 Concrete.Assign(
                   [tuple_elt],
                   Concrete.Name(tmp_name, Concrete.Load, annot),
                   annot)
               )
               elts (fst tmp_bindings)
           in
           verification @
           map_and_concat simplify_stmt assignment_list

         | Concrete.BoolOp _
         | Concrete.BinOp _
         | Concrete.UnaryOp _
           -> raise @@ Invalid_assignment "can't assign to operator"
         | Concrete.IfExp _
           -> raise @@ Invalid_assignment "can't assign to conditional expression"
         | Concrete.Compare _
           -> raise @@ Invalid_assignment "can't assign to comparison"
         | Concrete.Call _
           -> raise @@ Invalid_assignment "can't assign to function call"
         | Concrete.Num _
         | Concrete.Str _
         | Concrete.Bool _
           -> raise @@ Invalid_assignment "can't assign to literal"
         | Concrete.NoneExpr _
           -> raise @@ Invalid_assignment "cannot assign to None"
         | Concrete.Builtin _
           -> raise @@ Invalid_assignment "Can't assign to builtin. How did you even do that?"

      ) in
    [value_assignment] @ (map_and_concat simplify_assignment targets)

  | Concrete.AugAssign (target, op, value, annot) ->
    (* a += b "simplifies" to
       tmp1 = a
       try:
         tmp2 = tmp1.__iadd__
       except AttributeError:
         tmp2 = tmp1.__add__
       a = tmp2(b)

       except we have to do a little more work depending on the form of a.

       If a has the form obj.mem, we replace it with "tmp1 = obj; tmp1.mem += 2".
       This ensures that obj is only evaluated once, as required.

       Similarly, if a looks like list[slice], we evaluate the list and slice
       beforehand.
    *)
    let tmp1 = gen_unique_name annot in
    let tmp2 = gen_unique_name annot in
    let binds, newtarget =
      begin
        match target with
        | Concrete.Name _ ->
          [], target

        | Concrete.Attribute (obj, id, ctx, annot) ->
          [Simplified.Assign (tmp1, simplify_expr obj, annot)],
          Concrete.Attribute (Concrete.Name(tmp1, Concrete.Load, annot), id, ctx, annot)

        | Concrete.Subscript (lst, slice, ctx, annot) ->
          let slice_name = gen_unique_name annot in
          [
            Simplified.Assign (tmp1, simplify_expr lst, annot);
            Simplified.Assign (slice_name, simplify_slice slice annot, annot);
          ],
          Concrete.Subscript(Concrete.Name(tmp1, Concrete.Load, annot),
                             Concrete.Index(Concrete.Name(tmp2, Concrete.Load, annot)),
                             ctx,
                             annot)

        | Concrete.List _
        | Concrete.Tuple _
          -> raise @@ Invalid_assignment "illegal expression for augmented assignment"
        | Concrete.BoolOp _
        | Concrete.BinOp _
        | Concrete.UnaryOp _
          -> raise @@ Invalid_assignment "can't assign to operator"
        | Concrete.IfExp _
          -> raise @@ Invalid_assignment "can't assign to conditional expression"
        | Concrete.Compare _
          -> raise @@ Invalid_assignment "can't assign to comparison"
        | Concrete.Call _
          -> raise @@ Invalid_assignment "can't assign to function call"
        | Concrete.Num _
        | Concrete.Str _
        | Concrete.Bool _
          -> raise @@ Invalid_assignment "can't assign to literal"
        | Concrete.NoneExpr _
          -> raise @@ Invalid_assignment "cannot assign to None"
        | Concrete.Builtin _
          -> raise @@ Invalid_assignment "Can't assign to builtin. How did you even do that?"
      end in
    let tryexcept =
      Concrete.TryExcept(
        [
          Concrete.Assign(
            [Concrete.Name(tmp2, Concrete.Store, annot)],
            Concrete.Attribute(newtarget, simplify_augoperator op, Concrete.Load, annot),
            annot);
        ],
        [
          Concrete.ExceptHandler(
            Some(Concrete.Builtin(Builtin_AttributeError, annot)), None,
            [
              Concrete.Assign(
                [Concrete.Name(tmp2, Concrete.Store, annot)],
                Concrete.Attribute(newtarget, simplify_operator op, Concrete.Load, annot),
                annot)
            ],
            annot);
        ],
        [],
        annot);
    in
    let final_assign =
      Concrete.Assign([newtarget],
                      Concrete.Call(Concrete.Name(tmp2, Concrete.Load, annot),
                                    [value], [], None, None, annot),
                      annot)
    in
    binds @ simplify_stmt tryexcept @ simplify_stmt final_assign

  | Concrete.Print (dest, values, nl, annot) ->
    [Simplified.Print (
        simplify_expr_option dest,
        List.map simplify_expr values,
        nl,
        annot
      )]

  | Concrete.For (target, seq, body, _, annot) ->
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
      Concrete.Assign(
        [Concrete.Name(next_val, Concrete.Store, annot)],
        Concrete.Attribute(
          Concrete.Call(
            Concrete.Attribute(
              seq,
              "__iter__",
              Concrete.Load,
              annot
            ),
            [],
            [],
            None,
            None,
            annot
          ),
          "next",
          Concrete.Load,
          annot
        ),
        annot
      ) in
    let assign_target = (* i = next_val() *)
      Concrete.Assign(
        [target],
        Concrete.Call(
          Concrete.Name(next_val, Concrete.Load, annot),
          [],
          [],
          None,
          None,
          annot
        ),
        annot
      ) in
    let while_loop = (* while True: i = next_val(); <body> *)
      Concrete.While(
        Concrete.Bool(true, annot),
        [assign_target] @ body,
        [],
        annot
      ) in
    let try_except = (* try: <while>; except StopIteration: pass *)
      Concrete.TryExcept(
        [while_loop],
        [Concrete.ExceptHandler(
            Some(Concrete.Name("StopIteration", Concrete.Load, annot)),
            None,
            [Concrete.Pass(annot)],
            annot
          )],
        [],
        annot
      ) in
    map_and_concat simplify_stmt [bind_next_val; try_except]
  | Concrete.While (test, body, _, annot) ->
    [Simplified.While(simplify_expr test,
                      map_and_concat simplify_stmt body,
                      annot)]

  | Concrete.If (test, body, orelse, annot) ->
    [Simplified.If(simplify_expr test,
                   map_and_concat simplify_stmt body,
                   map_and_concat simplify_stmt orelse,
                   annot)]

  | Concrete.Raise (typ, _, _, annot) ->
    let simplified_typ =
      match typ with
      | None -> failwith "Raise must have exactly one argument"
      | Some(e) -> simplify_expr e
    in
    [Simplified.Raise(simplified_typ, annot)]

  | Concrete.TryExcept (body, handlers, _, annot) ->
    [Simplified.TryExcept (
        map_and_concat simplify_stmt body,
        List.map simplify_excepthandler handlers,
        annot)]

  | Concrete.Expr (e, annot) ->
    [Simplified.Expr(simplify_expr e, annot)]

  | Concrete.Pass (annot) ->
    [Simplified.Pass (annot)]

  | Concrete.Break (annot) ->
    [Simplified.Break (annot)]

  | Concrete.Continue (annot) ->
    [Simplified.Continue(annot)]

(* Given a concrete expr, returns a list of statements, corresponding to
   the assignments necessary to compute it, and the name of the final
   variable that was bound *)
and simplify_expr
    (e : 'a Concrete.expr)
  : 'a Simplified.expr =
  match e with
  | Concrete.BoolOp (op, values, annot) ->
    Simplified.BoolOp (simplify_boolop op,
                       List.map simplify_expr values,
                       annot)

  | Concrete.BinOp (left, op, right, annot) ->
    Simplified.Call(
      Simplified.Attribute(
        simplify_expr left,
        simplify_operator op,
        annot),
      [simplify_expr right],
      annot)

  | Concrete.UnaryOp (op, operand, annot) ->
    begin
      match op with
      | Concrete.Not ->
        Simplified.IfExp(
          simplify_expr operand,
          Simplified.Bool(false, annot),
          Simplified.Bool(true, annot),
          annot)

      | Concrete.UAdd ->
        Simplified.Call(
          Simplified.Attribute(
            simplify_expr operand,
            "__pos__",
            annot),
          [],
          annot)

      | Concrete.USub -> Simplified.Call(
          Simplified.Attribute(
            simplify_expr operand,
            "__neg__",
            annot),
          [],
          annot)
    end

  | Concrete.IfExp (test, body, orelse, annot) ->
    Simplified.IfExp (simplify_expr test,
                      simplify_expr body,
                      simplify_expr orelse,
                      annot)

  | Concrete.Compare (left, ops, comparators, annot) ->
    Simplified.Compare (simplify_expr left,
                        List.map simplify_cmpop ops,
                        List.map simplify_expr comparators,
                        annot)

  | Concrete.Call (func, args, _, _, _, annot) ->
    Simplified.Call (simplify_expr func,
                     List.map simplify_expr args,
                     annot)

  | Concrete.Num (n, annot) ->
    Simplified.Num(n, annot)

  | Concrete.Str (s, annot) ->
    Simplified.Str(s, annot)

  | Concrete.Bool (b, annot) ->
    Simplified.Bool(b, annot)

  | Concrete.Attribute (obj, attr, _, annot) ->
    Simplified.Attribute (simplify_expr obj, attr, annot)

  (* Turn subscripts into calls to __getitem__() *)
  | Concrete.Subscript (value, slice, _, annot) ->
    Simplified.Call(
      Simplified.Attribute(
        simplify_expr value,
        "__getitem__",
        annot),
      [simplify_slice slice annot],
      annot)

  | Concrete.Name (id, _, annot) -> (* Throw out context *)
    Simplified.Name(id, annot)

  | Concrete.List (elts, _, annot) ->
    Simplified.List (List.map simplify_expr elts, annot)

  | Concrete.Tuple (elts, _, annot) ->
    Simplified.Tuple (List.map simplify_expr elts, annot)

  | Concrete.NoneExpr (annot) ->
    Simplified.Name("*None", annot);

  | Concrete.Builtin (b, annot) ->
    Simplified.Builtin (b, annot)

and simplify_expr_option
    (o : 'a Concrete.expr option)
  : 'a Simplified.expr option =
  let simplified_opt = simplify_option simplify_expr o in
  match simplified_opt with
  | None -> None
  | Some(result) -> Some(result)

(* Turn a slice operator into a call to the slice() function, with
   the same arguments. E.g. 1:2:3 becomes slice(1,2,3), and
   1:2 becomes slice (1,2,None) *)
and simplify_slice
    (s : 'a Concrete.slice)
    annot
  : 'a Simplified.expr =
  (* Turn a "None" option into the python "None" object, and turn a
     "Some" option into the simplified version of its contents *)
  let exp_opt_to_slice_arg e =
    match e with
    | None ->
      Simplified.Name("*None", annot)
    | Some(x) ->
      simplify_expr x
  in
  match s with
  | Concrete.Slice (lower, upper, step) ->
    let args_list =
      [
        exp_opt_to_slice_arg lower;
        exp_opt_to_slice_arg upper;
        exp_opt_to_slice_arg step;
      ] in
    Simplified.Call(
      Simplified.Builtin(Builtin_slice, annot),
      args_list,
      annot
    )
  | Concrete.Index (value) ->
    simplify_expr value

and simplify_boolop b =
  match b with
  | Concrete.And -> Simplified.And
  | Concrete.Or -> Simplified.Or

and simplify_operator o =
  match o with
  | Concrete.Add  ->  "__add__"
  | Concrete.Sub  ->  "__sub__"
  | Concrete.Mult ->  "__mul__"
  | Concrete.Div  ->  "__div__"
  | Concrete.Mod  ->  "__mod__"
  | Concrete.Pow  ->  "__pow__"

and simplify_augoperator o =
  match o with
  | Concrete.Add  ->  "__iadd__"
  | Concrete.Sub  ->  "__isub__"
  | Concrete.Mult ->  "__imul__"
  | Concrete.Div  ->  "__idiv__"
  | Concrete.Mod  ->  "__imod__"
  | Concrete.Pow  ->  "__ipow__"


and simplify_cmpop o =
  match o with
  | Concrete.Eq -> Simplified.Eq
  | Concrete.NotEq -> Simplified.NotEq
  | Concrete.Lt -> Simplified.Lt
  | Concrete.LtE -> Simplified.LtE
  | Concrete.Gt -> Simplified.Gt
  | Concrete.GtE -> Simplified.GtE
  | Concrete.Is -> Simplified.Is
  | Concrete.IsNot -> Simplified.IsNot
  | Concrete.In -> Simplified.In
  | Concrete.NotIn -> Simplified.NotIn

and simplify_excepthandler h =
  match h with
  | Concrete.ExceptHandler (typ, name, body, annot) ->
    let new_typ =
      match typ with
      | None -> None
      | Some(e) -> Some(simplify_expr e)
    in
    let new_name =
      match name with
      | None -> None
      | Some(Concrete.Name(id,_,_)) -> Some(id)
      | _ -> failwith "Second argument to exception handler must be an identifier"
    in
    Simplified.ExceptHandler (
      new_typ,
      new_name,
      map_and_concat simplify_stmt body,
      annot)

and simplify_arguments a : identifier list =
  match a with
  | (args, _, _, _) ->
    List.map
      (fun arg ->
         match arg with
         | Concrete.Name (id, _, _) -> id
         | _ -> failwith "The arguments in a function definition must be identifiers"
      )
      args
