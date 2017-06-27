module Original = Python2_ast;;
module Concrete = Python2_concrete_ast;;
open Python2_ast_types;;

let rec generalize_modl = function
  | Original.Module (lst, annot) ->
    Concrete.Module (List.map generalize_stmt lst, annot)

and generalize_stmt = function
  | Original.FunctionDef (id, args, stmts, decs, annot) ->
    Concrete.FunctionDef (id,
                          generalize_arguments args,
                          List.map generalize_stmt stmts,
                          List.map generalize_expr decs,
                          annot)
  | Original.Return (value, annot) ->
    Concrete.Return (generalize_expr_option value, annot)
  | Original.Assign (targets, value, annot) ->
    Concrete.Assign (List.map generalize_expr targets,
                     generalize_expr value,
                     annot)
  | Original.AugAssign (target, op, value, annot) ->
    Concrete.AugAssign (generalize_expr target,
                        generalize_operator op,
                        generalize_expr value,
                        annot)
  | Original.Print (dest, values, nl, annot) ->
    Concrete.Print(generalize_expr_option dest,
                   List.map generalize_expr values,
                   nl,
                   annot)
  | Original.For (target, iter, body, orelse, annot) ->
    Concrete.For(generalize_expr target,
                 generalize_expr iter,
                 List.map generalize_stmt body,
                 List.map generalize_stmt orelse,
                 annot)
  | Original.While (test, body, orelse, annot) ->
    Concrete.While(generalize_expr test,
                   List.map generalize_stmt body,
                   List.map generalize_stmt orelse,
                   annot)
  | Original.If (test, body, orelse, annot) ->
    Concrete.If(generalize_expr test,
                List.map generalize_stmt body,
                List.map generalize_stmt orelse,
                annot)
  | Original.Raise (arg1, arg2, arg3, annot) ->
    Concrete.Raise(generalize_expr_option arg1,
                   generalize_expr_option arg2,
                   generalize_expr_option arg3,
                   annot)
  | Original.TryExcept (body, except, orelse, annot) ->
    Concrete.TryExcept (List.map generalize_stmt body,
                        List.map generalize_excepthandler except,
                        List.map generalize_stmt orelse,
                        annot)
  | Original.Expr (exp, annot) -> Concrete.Expr (generalize_expr exp, annot)
  | Original.Pass (annot)  -> Concrete.Pass (annot)
  | Original.Break (annot) -> Concrete.Break (annot)
  | Original.Continue (annot) -> Concrete.Continue (annot)

and generalize_expr = function
  | Original.BoolOp (op, values, annot) ->
    Concrete.BoolOp (generalize_boolop op,
                     List.map generalize_expr values,
                     annot)
  | Original.BinOp (left, op, right, annot) ->
    Concrete.BinOp(generalize_expr left,
                   generalize_operator op,
                   generalize_expr right,
                   annot)
  | Original.UnaryOp (op, operand, annot) ->
    Concrete.UnaryOp (generalize_unaryop op,
                      generalize_expr operand,
                      annot)
  | Original.IfExp (test, body, orelse, annot) ->
    Concrete.IfExp (generalize_expr test,
                    generalize_expr body,
                    generalize_expr orelse,
                    annot)
  | Original.Compare (left, ops, comps, annot) ->
    Concrete.Compare (generalize_expr left,
                      List.map generalize_cmpop ops,
                      List.map generalize_expr comps,
                      annot)
  | Original.Call (func, args, kwds, starargs, kwargs, annot) ->
    Concrete.Call (generalize_expr func,
                   List.map generalize_expr args,
                   List.map generalize_keyword kwds,
                   generalize_expr_option starargs,
                   generalize_expr_option kwargs,
                   annot)

  | Original.Num (n, annot) -> Concrete.Num(generalize_number n, annot)
  | Original.Str (s, annot) -> Concrete.Str(generalize_string s, annot)
  | Original.Bool (b, annot) -> Concrete.Bool(b, annot)
  | Original.Attribute (obj, id, ctx, annot) ->
    Concrete.Attribute(generalize_expr obj,
                       id,
                       generalize_expr_context ctx,
                       annot)
  | Original.Subscript (target, slice, ctx, annot) ->
    Concrete.Subscript (generalize_expr target,
                        generalize_slice slice,
                        generalize_expr_context ctx,
                        annot)
  | Original.Name (id, ctx, annot) ->
    Concrete.Name (id, generalize_expr_context ctx, annot)
  | Original.List (elts, ctx, annot) ->
    Concrete.List (List.map generalize_expr elts,
                   generalize_expr_context ctx,
                   annot)
  | Original.Tuple (elts, ctx, annot) ->
    Concrete.Tuple (List.map generalize_expr elts,
                    generalize_expr_context ctx,
                    annot)
  | Original.NoneExpr (annot) -> Concrete.NoneExpr (annot)

and generalize_expr_option = function
  | None -> None
  | Some(e) -> Some(generalize_expr e)

and generalize_expr_context = function
  | Original.Load -> Concrete.Load
  | Original.Store -> Concrete.Store
  | Original.Del -> Concrete.Del
  | Original.AugLoad -> Concrete.AugLoad
  | Original.AugStore -> Concrete.AugStore
  | Original.Param -> Concrete.Param

and generalize_slice = function
  | Original.Slice (lower, upper, step) ->
    Concrete.Slice (generalize_expr_option lower,
                    generalize_expr_option upper,
                    generalize_expr_option step)
  | Original.Index (value) ->
    Concrete.Index(generalize_expr value)

and generalize_boolop = function
  | Original.And -> Concrete.And
  | Original.Or -> Concrete.Or

and generalize_operator = function
  | Original.Add -> Concrete.Add
  | Original.Sub -> Concrete.Sub
  | Original.Mult -> Concrete.Mult
  | Original.Div -> Concrete.Div
  | Original.Mod -> Concrete.Mod
  | Original.Pow -> Concrete.Pow

and generalize_unaryop = function
  | Original.Not -> Concrete.Not
  | Original.UAdd -> Concrete.UAdd
  | Original.USub -> Concrete.USub

and generalize_cmpop = function
  | Original.Eq -> Concrete.Eq
  | Original.NotEq -> Concrete.NotEq
  | Original.Lt -> Concrete.Lt
  | Original.LtE -> Concrete.LtE
  | Original.Gt -> Concrete.Gt
  | Original.GtE -> Concrete.GtE
  | Original.Is -> Concrete.Is
  | Original.IsNot -> Concrete.IsNot
  | Original.In -> Concrete.In
  | Original.NotIn -> Concrete.NotIn


and generalize_excepthandler = function
  | Original.ExceptHandler (typ, name, body, annot ) ->
    Concrete.ExceptHandler (generalize_expr_option typ,
                            generalize_expr_option name,
                            List.map generalize_stmt body,
                            annot)

and generalize_arguments a =
  let args, varargs, kwargs, defaults = a in
  List.map generalize_expr args,
  varargs,
  kwargs,
  List.map generalize_expr defaults

and generalize_keyword k =
  let id, exp = k in
  id, generalize_expr exp

and generalize_number num : Python2_ast_types.number =
  match num with
  | Original.Int (n)
  | Original.LongInt (n) -> Int(n)
  | Original.Float(f) -> Float(f)

and generalize_string str = StringLiteral(str)
