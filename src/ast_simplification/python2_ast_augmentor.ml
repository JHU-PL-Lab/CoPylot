open Batteries;;
open Python2_ast_types;;
module Base = Python2_ast;;
module Augmented = Python2_augmented_ast;;

let rec augment_modl (m : annot Base.modl) : annot Augmented.modl =
  let Base.Module (body, annot) = m in
  Augmented.Module(List.map augment_stmt body, annot)

and augment_stmt (s : annot Base.stmt) : annot Augmented.stmt =
  match s with
  | Base.FunctionDef (name, args, body, _, annot) ->
    Augmented.FunctionDef (name,
                           augment_args args,
                           List.map augment_stmt body,
                           annot)
  | Base.Return (val_opt, annot) ->
    Augmented.Return (Option.map augment_expr val_opt, annot)
  | Base.Assign (targets, value, annot) ->
    Augmented.Assign (List.map augment_expr targets,
                      augment_expr value,
                      annot)
  | Base.AugAssign (target, op, value, annot) ->
    Augmented.AugAssign (augment_expr target,
                         augment_binop op,
                         augment_expr value,
                         annot)
  | Base.Print _ -> failwith "Print statements not supported"
  | Base.For (target, iter, body, orelse, annot) ->
    Augmented.For (augment_expr target,
                   augment_expr iter,
                   List.map augment_stmt body,
                   List.map augment_stmt orelse,
                   annot)
  | Base.While (test, body, orelse, annot) ->
    Augmented.While (augment_expr test,
                     List.map augment_stmt body,
                     List.map augment_stmt orelse,
                     annot)
  | Base.If (test, body, orelse, annot) ->
    Augmented.If (augment_expr test,
                  List.map augment_stmt body,
                  List.map augment_stmt orelse,
                  annot)
  | Base.Raise (typ, _, _, annot) ->
    Augmented.Raise (Option.map augment_expr typ, annot)
  | Base.TryExcept (body, handlers, orelse, annot) ->
    Augmented.TryExcept (List.map augment_stmt body,
                         List.map augment_handler handlers,
                         List.map augment_stmt orelse,
                         annot)
  | Base.Expr (e, annot) ->
    Augmented.Expr (augment_expr e, annot)
  | Base.Pass annot ->
    Augmented.Pass annot
  | Base.Break annot ->
    Augmented.Break annot
  | Base.Continue annot ->
    Augmented.Continue annot

and augment_expr (e : annot Base.expr) : annot Augmented.expr =
  match e with
  | Base.BoolOp (op, vals, annot) ->
    Augmented.BoolOp(augment_boolop op,
                     List.map augment_expr vals,
                     annot)
  | Base.BinOp (left, op, right, annot) ->
    Augmented.BinOp (augment_expr left,
                     augment_binop op,
                     augment_expr right,
                     annot)
  | Base.UnaryOp (op, arg, annot) ->
    Augmented.UnaryOp (augment_unop op,
                       augment_expr arg,
                       annot)
  | Base.IfExp (test, body, orelse, annot) ->
    Augmented.IfExp (augment_expr test,
                     augment_expr body,
                     augment_expr orelse,
                     annot)
  | Base.Compare (left, ops, rest, annot) ->
    Augmented.Compare (augment_expr left,
                       List.map augment_cmpop ops,
                       List.map augment_expr rest,
                       annot)
  | Base.Call (func, args, _, _, _, annot) ->
    Augmented.Call (augment_expr func,
                    List.map augment_expr args,
                    annot)
  | Base.Attribute (obj, attr, _, annot) ->
    Augmented.Attribute (augment_expr obj, attr, annot)
  | Base.Subscript (lst, slice, _, annot) ->
    Augmented.Subscript (augment_expr lst,
                         augment_slice slice,
                         annot)
  | Base.Name (id, _, annot) ->
    Augmented.Name (id, annot)
  | Base.List (lst, _, annot) ->
    Augmented.List (List.map augment_expr lst, annot)
  | Base.Tuple (lst, _, annot) ->
    Augmented.Tuple (List.map augment_expr lst, annot)
  | Base.Num (n, annot) ->
    Augmented.Num (augment_number n, annot)
  | Base.Str (s, annot) ->
    Augmented.Str (s, annot)
  | Base.Bool (b, annot) ->
    Augmented.Bool (b, annot)
  | Base.NoneExpr annot ->
    Augmented.NoneExpr annot

and augment_args (args : annot Base.arguments) : identifier list =
  let (ids, _, _, _) = args in
  List.map
    (function
      | Base.Name(id, _, _) -> id
      | _ -> failwith "Tried to use non-identifers for parameters in a function definition"
    )
    ids

and augment_handler = function
  | Base.ExceptHandler(typ, name, body, annot) ->
    Augmented.ExceptHandler(Option.map augment_expr typ,
                            Option.map augment_expr name,
                            List.map augment_stmt body,
                            annot)

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
  | Base.Not -> Augmented.Not
  | Base.UAdd -> Augmented.UAdd
  | Base.USub -> Augmented.USub

and augment_binop = function
  | Base.Add -> Augmented.Add
  | Base.Sub -> Augmented.Sub
  | Base.Mult -> Augmented.Mult
  | Base.Div -> Augmented.Div
  | Base.Mod -> Augmented.Mod
  | Base.Pow -> Augmented.Pow

and augment_boolop = function
  | Base.And -> Augmented.And
  | Base.Or -> Augmented.Or

and augment_cmpop = function
  | Base.Eq -> Augmented.Eq
  | Base.NotEq -> Augmented.NotEq
  | Base.Lt -> Augmented.Lt
  | Base.LtE -> Augmented.LtE
  | Base.Gt -> Augmented.Gt
  | Base.GtE -> Augmented.GtE
  | Base.Is -> Augmented.Is
  | Base.IsNot -> Augmented.IsNot
  | Base.In -> Augmented.In
  | Base.NotIn -> Augmented.NotIn
;;
