open Python2_ast_types;;
module Simplified = Python2_simplified_ast;;
module Normalized = Python2_normalized_ast;;
open Unique_name_ctx;;

(* FIXME: We need to create a type for builtin methods (in addition to builtin
   functions such as type, bool, slice) such as __getattr__, and use that
   instead of just a string whenever we invoke them in this file *)

let map_and_concat (func : 'a -> 'b list) (lst : 'a list) =
  List.concat (List.map func lst)
;;

let annotate_stmt annot s = {annot = annot; body = s};;
let annotate_expr annot e = {annot = annot; body = e};;

(* Given a uid and an annotated_expr, assigns that expr to a new, unique name.
   Returns the assignment statement (in a list) and the name used *)
let gen_normalized_assignment ctx annot
    (e : Normalized.expr) =
  let name = gen_unique_name ctx annot in
  [
    annotate_stmt annot @@
    Normalized.Assign(name, annotate_expr annot e)
  ],
  name
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

let rec normalize_modl (ctx : name_context) m : Normalized.modl =
  match m with
  | Simplified.Module (body, _) ->
    let normalized_prog = map_and_concat (normalize_stmt ctx) body in
    Normalized.Module(normalized_prog)

and normalize_stmt ctx
    (s : 'a Simplified.stmt)
  : Normalized.annotated_stmt list =
  let normalize_stmt = normalize_stmt ctx in
  let normalize_expr = normalize_expr ctx in
  match s with
  | Simplified.Return (value, annot) ->
    let bindings, result = normalize_expr value in
    bindings @
    [ annotate_stmt annot @@ Normalized.Return(result)]


  | Simplified.Assign (target, value, annot) ->
    let value_bindings, value_result = normalize_expr value in
    let assign =
      annotate_stmt annot @@
      Normalized.Assign(target,
                        annotate_expr annot @@
                        Normalized.Name(value_result))
    in
    value_bindings @ [assign]

  | Simplified.While (test, body, annot) ->
    [annotate_stmt annot @@
     Normalized.While(test,
                      map_and_concat normalize_stmt body)]

  | Simplified.If (test, body, orelse, annot) ->
    let test_bindings, test_result = normalize_expr test in
    test_bindings @
    [annotate_stmt annot @@
     Normalized.If (test_result,
                    map_and_concat normalize_stmt body,
                    map_and_concat normalize_stmt orelse)]

  | Simplified.Raise (value, annot) ->
    let value_binding, value_result = normalize_expr value in
    value_binding @
    [ annotate_stmt annot @@ Normalized.Raise(value_result)]

  | Simplified.TryExcept (body, exn_name, handler, annot) ->
    [annotate_stmt annot @@
     Normalized.TryExcept (map_and_concat normalize_stmt body,
                           exn_name,
                           map_and_concat normalize_stmt handler)]

  | Simplified.Pass annot ->
    [ annotate_stmt annot @@ Normalized.Pass]

  | Simplified.Break annot ->
    [ annotate_stmt annot @@ Normalized.Break ]

  | Simplified.Continue annot ->
    [ annotate_stmt annot @@ Normalized.Continue ]

  | Simplified.Expr (e, _) ->
    let bindings, _ =
      match e with
      (* If this is just a name, we generate a useless assignment so that
         we can fit it into an assignment statement format. For all other
         exprs, normalize_expr will do this for us *)
      | Simplified.Name (id, annot) ->
        gen_normalized_assignment ctx annot @@
        Normalized.Name(id)
      | _ ->
        normalize_expr e
    in
    bindings

(* Given a simplified expr, returns a list of statements, corresponding to
   the assignments necessary to compute it, and the name of the final
   variable that was bound *)
and normalize_expr ctx
    (e : 'a Simplified.expr)
  : Normalized.annotated_stmt list * identifier =
  let normalize_stmt = normalize_stmt ctx in
  let normalize_expr = normalize_expr ctx in
  match e with
  | Simplified.UnaryOp(op, value, annot) ->
    let value_bindings, value_result = normalize_expr value in
    let bindings, result =
      gen_normalized_assignment ctx annot @@
      Normalized.UnaryOp(normalize_unaryop op, value_result)
    in
    value_bindings @ bindings, result

  | Simplified.Binop(left, op, right, annot) ->
    let left_bindings, left_result = normalize_expr left in
    let right_bindings, right_result = normalize_expr right in
    let bindings, result =
      gen_normalized_assignment ctx annot @@
      Normalized.Binop(left_result, normalize_binop op, right_result)
    in
    left_bindings @ right_bindings @ bindings, result

  | Simplified.Call (func, args, annot) ->
    (* In order to call an object, we must first get its __call__ attribute.
       If this does not exist, we throw a type error. Otherwise, we continue
       to retrieve the __call__ attribute until we get a method wrapper,
       which we know how to call. *)
    let func_bindings, func_name = normalize_expr func in
    let arg_bindings, arg_names = normalize_list normalize_expr args in
    let assignment, name =
      gen_normalized_assignment ctx annot @@
      Normalized.Call(func_name, arg_names)
    in
    let bindings = func_bindings @ arg_bindings @ assignment in
    bindings, name

  | Simplified.Num (n, annot) ->
    gen_normalized_assignment ctx annot @@
    Normalized.Num(n)

  | Simplified.Str (s, annot) ->
    gen_normalized_assignment ctx annot @@
    Normalized.Str(s)

  | Simplified.Bool (b, annot) ->
    gen_normalized_assignment ctx annot @@
    Normalized.Bool(b)

  | Simplified.Builtin (b, annot) ->
    gen_normalized_assignment ctx annot @@
    Normalized.Builtin(b)

  | Simplified.FunctionVal (args, body, annot) ->
    gen_normalized_assignment ctx annot @@
    Normalized.FunctionVal(args,
                           map_and_concat normalize_stmt body)

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
    let obj_bindings, obj_result = normalize_expr obj in
    let bindings, result =
      gen_normalized_assignment ctx annot @@
      Normalized.Attribute(obj_result, attr)
    in
    obj_bindings @ bindings, result

  | Simplified.Name (id, _) ->
    ([], id)

  | Simplified.List (elts, annot) ->
    let bindings, results = normalize_list normalize_expr elts in
    let assignment, name =
      gen_normalized_assignment ctx annot @@
      Normalized.List(results) in
    bindings @ assignment, name

  | Simplified.Tuple (elts, annot) ->
    let bindings, results = normalize_list normalize_expr elts in
    let assignment, name =
      gen_normalized_assignment ctx annot @@
      Normalized.Tuple(results) in
    bindings @ assignment, name

and normalize_binop o =
  match o with
  | Simplified.Is -> Normalized.Is

and normalize_unaryop o =
  match o with
  | Simplified.Not -> Normalized.Not
