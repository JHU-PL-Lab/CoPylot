module Abstract = Python2_abstract_ast
module Simplified = Python2_simplified_ast
exception Identifier_Only

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

let rec simplify_modl (m : 'a Abstract.modl) : 'a Simplified.modl =
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
    [Simplified.Assign(func_name,
                       Simplified.FunctionVal(
                         simplified_args,
                         simplified_body,
                         annot),
                       annot)]

  | Abstract.Return (value, annot) ->
    [Simplified.Return(simplify_expr_option value, annot)]

  | Abstract.Assign (targets, value, annot) ->
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
         | Abstract.Name (id, _, _) ->
           [Simplified.Assign(id,
                              value_name,
                              annot)]

         | Abstract.Attribute (obj, id, _, _) ->
           [Simplified.Expr(
               Simplified.Call(
                 Simplified.Attribute(
                   simplify_expr obj,
                   "__setattr__",
                   annot),
                 [
                   Simplified.Str(Simplified.StringLiteral(id), annot);
                   value_name;
                 ],
                 annot),
               annot)]

         | Abstract.Subscript (lst, slice, _, _) ->
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
         | Abstract.List (elts, _, _)
         | Abstract.Tuple (elts, _, _) ->
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
                 (prev : Abstract.identifier list * 'a Simplified.stmt list)
                 (_ : 'a Abstract.expr) ->
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
                     Simplified.Name("ValueError", annot),
                     [Simplified.Str(
                         Simplified.StringLiteral("too many values to unpack"),
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
                         Simplified.Name("ValueError", annot),
                         [Simplified.Str(
                             Simplified.StringAbstract,
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
                 (tuple_elt : 'a Abstract.expr)
                 (tmp_name : Abstract.identifier) ->
                 Abstract.Assign(
                   [tuple_elt],
                   Abstract.Name(tmp_name, Abstract.Load, annot),
                   annot)
               )
               elts (fst tmp_bindings)
           in
           verification @
           map_and_concat simplify_stmt assignment_list

         | Abstract.BoolOp _
         | Abstract.BinOp _
         | Abstract.UnaryOp _ -> failwith "can't assign to operator"
         | Abstract.IfExp _ -> failwith "can't assign to conditional expression"
         | Abstract.Compare _ -> failwith "can't assign to comparison"
         | Abstract.Call _ -> failwith "can't assign to function call"
         | Abstract.Num _
         | Abstract.Str _
         | Abstract.Bool _ -> failwith "can't assign to literal"
      ) in
    [value_assignment] @ (map_and_concat simplify_assignment targets)

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
        Abstract.Bool(true, annot),
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

  | Abstract.Raise (typ, _, _, annot) ->
    let simplified_typ =
      match typ with
      | None -> failwith "Raise must have exactly one argument"
      | Some(e) -> simplify_expr e
    in
    [Simplified.Raise(simplified_typ, annot)]

  | Abstract.TryExcept (body, handlers, _, annot) ->
    [Simplified.TryExcept (
        map_and_concat simplify_stmt body,
        List.map simplify_excepthandler handlers,
        annot)]

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
    Simplified.Call(
      Simplified.Attribute(
        simplify_expr left,
        simplify_operator op,
        annot),
      [simplify_expr right],
      annot)

  | Abstract.UnaryOp (op, operand, annot) ->
    begin
      match op with
      | Abstract.Not ->
        Simplified.IfExp(
          simplify_expr operand,
          Simplified.Bool(false, annot),
          Simplified.Bool(true, annot),
          annot)

      | Abstract.UAdd ->
        Simplified.Call(
          Simplified.Attribute(
            simplify_expr operand,
            "__pos__",
            annot),
          [],
          annot)

      | Abstract.USub -> Simplified.Call(
          Simplified.Attribute(
            simplify_expr operand,
            "__neg__",
            annot),
          [],
          annot)
    end

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

  | Abstract.Str (typ, annot) ->
    Simplified.Str(simplify_str typ, annot)

  | Abstract.Bool (b, annot) ->
    Simplified.Bool(b, annot)

  | Abstract.Attribute (obj, attr, _, annot) ->
    Simplified.Attribute (simplify_expr obj, attr, annot)

  (* Turn subscripts into calls to __getitem__() *)
  | Abstract.Subscript (value, slice, _, annot) ->
    Simplified.Call(
      Simplified.Attribute(
        simplify_expr value,
        "__getitem__",
        annot),
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
  match s with
  | Abstract.Slice (lower, upper, step) ->
    let args_list =
      [
        exp_opt_to_slice_arg lower;
        exp_opt_to_slice_arg upper;
        exp_opt_to_slice_arg step;
      ] in
    Simplified.Call(
      Simplified.Builtin(Simplified.Builtin_slice, annot),
      args_list,
      annot
    )
  | Abstract.Index (value) ->
    simplify_expr value

and simplify_boolop b =
  match b with
  | Abstract.And -> Simplified.And
  | Abstract.Or -> Simplified.Or

and simplify_operator o =
  match o with
  | Abstract.Add -> "__add__"
  | Abstract.Sub -> "__sub__"
  | Abstract.Mult -> "__mul__"
  | Abstract.Div -> "__div__"
  | Abstract.Mod -> "__mod__"
  | Abstract.Pow -> "__pow__"

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

and simplify_excepthandler h =
  match h with
  | Abstract.ExceptHandler (typ, name, body, annot) ->
    let new_typ =
      match typ with
      | None -> None
      | Some(e) -> Some(simplify_expr e)
    in
    let new_name =
      match name with
      | None -> None
      | Some(Abstract.Name(id,_,_)) -> Some(id)
      | _ -> failwith "Second argument to exception handler must be an identifier"
    in
    Simplified.ExceptHandler (
      new_typ,
      new_name,
      map_and_concat simplify_stmt body,
      annot)

and simplify_arguments a : Simplified.identifier list =
  match a with
  | (args, _, _, _) ->
    List.map
      (fun arg ->
         match arg with
         | Abstract.Name (id, _, _) -> id
         | _ -> failwith "The arguments in a function definition must be identifiers"
      )
      args

and simplify_sign s =
  match s with
  | Abstract.Pos -> Simplified.Pos
  | Abstract.Neg -> Simplified.Neg
  | Abstract.Zero -> Simplified.Zero

and simplify_number n =
  match n with
  | Abstract.Int sgn -> Simplified.Int(simplify_sign sgn)
  | Abstract.Float sgn -> Simplified.Float(simplify_sign sgn)

and simplify_str s =
  match s with
  | Abstract.StringAbstract -> Simplified.StringAbstract
  | Abstract.StringLiteral (s) -> Simplified.StringLiteral (s)
