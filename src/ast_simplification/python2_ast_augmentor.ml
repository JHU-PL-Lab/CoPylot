open Batteries;;
open Python2_ast_types;;
module Base = Python2_ast;;
module Augmented = Python2_augmented_ast;;

let rec augment_modl (m : annot Base.modl) : Augmented.modl =
  let Base.Module (body, _) = m in
  Augmented.Module(List.map augment_stmt body)

and augment_stmt (s : annot Base.stmt) : Augmented.annotated_stmt =
  match s with
  | Base.FunctionDef (name, args, body, _, annot) ->
    annotate annot @@
    Augmented.FunctionDef (name,
                           augment_args args,
                           List.map augment_stmt body)
  | Base.Return (val_opt, annot) ->
    annotate annot @@
    Augmented.Return (Option.map augment_expr val_opt)
  | Base.Assign (targets, value, annot) ->
    annotate annot @@
    Augmented.Assign (List.map augment_expr targets,
                      augment_expr value)
  | Base.AugAssign (target, op, value, annot) ->
    annotate annot @@
    Augmented.AugAssign (augment_expr target,
                         augment_binop op,
                         augment_expr value)
  | Base.Print _ -> failwith "Print statements not supported"
  | Base.For (target, iter, body, orelse, annot) ->
    annotate annot @@
    Augmented.For (augment_expr target,
                   augment_expr iter,
                   List.map augment_stmt body,
                   List.map augment_stmt orelse)
  | Base.While (test, body, orelse, annot) ->
    annotate annot @@
    Augmented.While (augment_expr test,
                     List.map augment_stmt body,
                     List.map augment_stmt orelse)
  | Base.If (test, body, orelse, annot) ->
    annotate annot @@
    Augmented.If (augment_expr test,
                  List.map augment_stmt body,
                  List.map augment_stmt orelse)
  | Base.Raise (typ, _, _, annot) ->
    annotate annot @@
    Augmented.Raise (Option.map augment_expr typ)
  | Base.TryExcept (body, handlers, orelse, annot) ->
    annotate annot @@
    Augmented.TryExcept (List.map augment_stmt body,
                         List.map augment_handler handlers,
                         List.map augment_stmt orelse)
  | Base.Expr (e, annot) ->
    annotate annot @@
    Augmented.Expr (augment_expr e)
  | Base.Pass annot ->
    annotate annot @@
    Augmented.Pass
  | Base.Break annot ->
    annotate annot @@
    Augmented.Break
  | Base.Continue annot ->
    annotate annot @@
    Augmented.Continue

and augment_expr (e : annot Base.expr) : Augmented.annotated_expr =
  match e with
  | Base.BoolOp (op, vals, annot) ->
    annotate annot @@
    Augmented.BoolOp(augment_boolop op,
                     List.map augment_expr vals)
  | Base.BinOp (left, op, right, annot) ->
    annotate annot @@
    Augmented.BinOp (augment_expr left,
                     augment_binop op,
                     augment_expr right)
  | Base.UnaryOp (op, arg, annot) ->
    annotate annot @@
    Augmented.UnaryOp (augment_unop op,
                       augment_expr arg)
  | Base.IfExp (test, body, orelse, annot) ->
    annotate annot @@
    Augmented.IfExp (augment_expr test,
                     augment_expr body,
                     augment_expr orelse)
  | Base.Compare (left, ops, rest, annot) ->
    annotate annot @@
    Augmented.Compare (augment_expr left,
                       List.map augment_cmpop ops,
                       List.map augment_expr rest)
  | Base.Call (func, args, _, _, _, annot) ->
    annotate annot @@
    Augmented.Call (augment_expr func,
                    List.map augment_expr args)
  | Base.Attribute (obj, attr, _, annot) ->
    annotate annot @@
    Augmented.Attribute (augment_expr obj, attr)
  | Base.Subscript (lst, slice, _, annot) ->
    annotate annot @@
    Augmented.Subscript (augment_expr lst,
                         augment_slice slice)
  | Base.Name (id, _, annot) ->
    annotate annot @@
    Augmented.Name (id)
  | Base.List (lst, _, annot) ->
    annotate annot @@
    Augmented.List (List.map augment_expr lst)
  | Base.Tuple (lst, _, annot) ->
    annotate annot @@
    Augmented.Tuple (List.map augment_expr lst)
  | Base.Num (n, annot) ->
    annotate annot @@
    Augmented.Num (augment_number n)
  | Base.Str (s, annot) ->
    annotate annot @@
    Augmented.Str (s)
  | Base.Bool (b, annot) ->
    annotate annot @@
    Augmented.Bool (b)
  | Base.NoneExpr annot ->
    annotate annot @@
    Augmented.NoneExpr

and augment_args (args : annot Base.arguments) : identifier list =
  let (ids, _, _, _) = args in
  List.map
    (function
      | Base.Name(id, _, _) -> id
      | _ -> failwith "Tried to use non-identifers for parameters in a function definition"
    )
    ids

and augment_handler = function
  | Base.ExceptHandler(typ, name, body, _) ->
    Augmented.ExceptHandler(Option.map augment_expr typ,
                            Option.map augment_expr name,
                            List.map augment_stmt body)

and augment_slice = function
  | Base.Slice (lower, upper, step) ->
    Augmented.Slice (Option.map augment_expr lower,
                     Option.map augment_expr upper,
                     Option.map augment_expr step)
  | Base.Index i ->
    Augmented.Index (augment_expr i)

and augment_number = function
  | Base.Int n
  | Base.LongInt n -> Int n
  | Base.Float f -> Float f

and augment_unop = function
  | Base.Not ->
    Augmented.Not
  | Base.UAdd ->
    Augmented.UAdd
  | Base.USub ->
    Augmented.USub

and augment_binop = function
  | Base.Add ->
    Augmented.Add
  | Base.Sub ->
    Augmented.Sub
  | Base.Mult ->
    Augmented.Mult
  | Base.Div ->
    Augmented.Div
  | Base.Mod ->
    Augmented.Mod
  | Base.Pow ->
    Augmented.Pow

and augment_boolop = function
  | Base.And ->
    Augmented.And
  | Base.Or ->
    Augmented.Or

and augment_cmpop = function
  | Base.Eq ->
    Augmented.Eq
  | Base.NotEq ->
    Augmented.NotEq
  | Base.Lt ->
    Augmented.Lt
  | Base.LtE ->
    Augmented.LtE
  | Base.Gt ->
    Augmented.Gt
  | Base.GtE ->
    Augmented.GtE
  | Base.Is ->
    Augmented.Is
  | Base.IsNot ->
    Augmented.IsNot
  | Base.In ->
    Augmented.In
  | Base.NotIn ->
    Augmented.NotIn
;;
