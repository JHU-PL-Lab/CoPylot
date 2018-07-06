open Python2_ast_types;;
module Simplified = Python2_simplified_ast;;
module Normalized = Python2_normalized_ast;;

open Python_normalization_monad;;
open Normalization_monad;;

(* Given a uid and an annotated_expr, assigns that expr to a new, unique name.
   Returns the assignment statement (in a list) and the name used *)
(* let gen_normalized_assignment ctx annot
    (e : Normalized.expr) =
  let name = gen_unique_name ctx annot in
  [
    annotate annot @@
    Normalized.Assign(name, annotate annot e)
  ],
  name
;; *)

let rec normalize_modl
    (ctx : Unique_name_ctx.name_context)
    (m : Simplified.annotated_modl)
  : Normalized.annotated_modl =
  let Simplified.Module(body) = m.body in
  let _, normalized_prog = run ctx m.annot @@ normalize_stmt_list body in
  annotate m.annot @@
  Normalized.Module(normalized_prog)

and normalize_stmt_list (stmts : Simplified.annotated_stmt list) : unit m =
  let%bind _ = sequence @@ List.map normalize_stmt stmts in
  return ()

and normalize_stmt (s : Simplified.annotated_stmt) : unit m =
  local_annot s.annot @@
  match s.body with
  | Simplified.Return (value) ->
    emit [ Normalized.Return(value) ]

  | Simplified.Assign (target, value) ->
    let%bind value_result = normalize_expr value in
    emit [ Normalized.Assign(target, value_result) ]

  | Simplified.While (test, body, orelse) ->
    let%bind _, normalized_body = listen @@ normalize_stmt_list body in
    let%bind _, normalized_orelse = listen @@ normalize_stmt_list orelse in
    emit
      [
        Normalized.While(test, normalized_body, normalized_orelse)
      ]

  | Simplified.If (test, body, orelse) ->
    let%bind _, normalized_body = listen @@ normalize_stmt_list body in
    let%bind _, normalized_orelse = listen @@ normalize_stmt_list orelse in
    emit
      [
        Normalized.If(test, normalized_body, normalized_orelse)
      ]

  | Simplified.Raise (value) ->
    emit [ Normalized.Raise(value) ]

  | Simplified.TryExcept (body, exn_name, handler, orelse) ->
    let%bind _, normalized_body = listen @@ normalize_stmt_list body in
    let%bind _, normalized_handler = listen @@ normalize_stmt_list handler in
    let%bind _, normalized_orelse = listen @@ normalize_stmt_list orelse in
    emit
      [
        Normalized.TryExcept(normalized_body,
                             exn_name,
                             normalized_handler,
                             normalized_orelse)
      ]

  | Simplified.Pass ->
    emit [ Normalized.Pass]

  | Simplified.Break ->
    emit [ Normalized.Break ]

  | Simplified.Continue ->
    emit [ Normalized.Continue ]

(* normalize_expr is ONLY called in the contest of an assignment (from a Simplified.Assign).
   We return the expr which we want to get bound to the target of that assignment.
   For most types of expr, this is trivial. *)
and normalize_expr (e : Simplified.annotated_expr) : Normalized.annotated_expr m =
  local_annot e.annot @@
  let annotate body = annotate e.annot body in
  let ret expr = return @@ annotate @@ expr in
  match e.body with
  | Simplified.UnaryOp (op, value) ->
    ret @@ Normalized.UnaryOp(normalize_unaryop op, value)

  | Simplified.Binop (left, op, right) ->
    ret @@ Normalized.Binop(left, normalize_binop op, right)

  | Simplified.Call (func, args) ->
    (* TODO: Do we do this here?
       In order to call an object, we must first get its __call__ attribute.
       If this does not exist, we throw a type error. Otherwise, we continue
       to retrieve the __call__ attribute until we get a method wrapper,
       which we know how to call. *)
    ret @@ Normalized.Call(func, args)

  | Simplified.Num (n) ->
    ret @@ Normalized.Num(n)

  | Simplified.Str (s) ->
    ret @@ Normalized.Str(s)

  | Simplified.Bool (b) ->
    ret @@ Normalized.Bool(b)

  | Simplified.Builtin (b) ->
    ret @@ Normalized.Builtin(b)

  | Simplified.FunctionVal (args, body) ->
    let%bind _, normalized_body = listen @@ normalize_stmt_list body in
    ret @@ Normalized.FunctionVal(args, normalized_body)

  | Simplified.Attribute (obj, attr) ->
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
    (*
    TODO: This happens when converting to lamia
    let obj_bindings, obj_result = normalize_expr obj in
    let obj_name = Simplified.Name(obj_result) in
    let getattribute_name = gen_unique_name ctx annot in
    let getattr_name = gen_unique_name ctx annot in
    let exn_name = gen_unique_name ctx annot in

    let getattr_try =
      Simplified.TryExcept(
        [
          Simplified.Assign(getattr_name,
                            Simplified.SimpleAttribute(obj_name, "__getattr__"),
                            annot);
        ],
        [
          Simplified.ExceptHandler(
            Some(Simplified.Builtin(Builtin_AttributeError)),
            None,
            [ Simplified.Raise(Simplified.Name(exn_name)) ],
            annot);
        ],
        annot)
    in
    let result_name = gen_unique_name ctx annot in
    let overall_try =
      Simplified.TryExcept(
        [
          Simplified.Assign(getattribute_name,
                            Simplified.SimpleAttribute(obj_name, "__getattribute__"),
                            annot);
          Simplified.Assign(result_name,
                            Simplified.Call(Simplified.Name(getattribute_name),
                                            [Simplified.Str(StringLiteral(attr))],
                                            annot),
                            annot);
        ],
        [
          Simplified.ExceptHandler(
            Some(Simplified.Builtin(Builtin_AttributeError)),
            Some(exn_name),
            [
              getattr_try;
              Simplified.Assign(result_name,
                                Simplified.Call(
                                  Simplified.Name(getattr_name),
                                  [Simplified.Str(StringLiteral(attr))],
                                  annot),
                                annot);
            ],
            annot);
        ],
        annot)
    in
    let normalized_try = normalize_stmt overall_try in
    obj_bindings @ normalized_try, result_name *)
    ret @@ Normalized.Attribute(obj, attr)

  | Simplified.Name (id) ->
    ret @@ Normalized.Name(id)

  | Simplified.List (elts) ->
    ret @@ Normalized.List(elts)

  | Simplified.Tuple (elts) ->
    ret @@ Normalized.Tuple(elts)

and normalize_binop o =
  match o with
  | Simplified.Is -> Normalized.Is

and normalize_unaryop o =
  match o with
  | Simplified.Not -> Normalized.Not
