module Abstract = Python2_abstract_ast
module Concrete = Python2_ast

(* Lets us lift optional types easily *)
let lift_option func opt =
  match opt with
  | None -> None
  | Some(x) -> Some(func x)

let rec lift_modl m =
  match m with
  | Concrete.Module (statements, annot) ->
    Abstract.Module(List.map lift_stmt statements, annot)

and lift_stmt s =
  match s with
  | Concrete.FunctionDef (id, args, body, decorators, annot) ->
    Abstract.FunctionDef (id, lift_arguments args, List.map lift_stmt body,
                          List.map lift_expr decorators, annot)
  | Concrete.Return (arg1, annot) ->
    Abstract.Return (lift_option lift_expr arg1, annot)
  | Concrete.Assign (arg1, arg2, annot) ->
    Abstract.Assign (List.map lift_expr arg1, lift_expr arg2, annot)
  | Concrete.AugAssign (arg1, arg2, arg3, annot) ->
    Abstract.AugAssign (lift_expr arg1, lift_operator arg2, lift_expr arg3, annot)
  | Concrete.Print (arg1, arg2, arg3, annot) ->
    Abstract.Print (lift_option lift_expr arg1, List.map lift_expr arg2, arg3, annot)
  | Concrete.For (arg1, arg2, arg3, arg4, annot) ->
    Abstract.For (lift_expr arg1, lift_expr arg2, List.map lift_stmt arg3,
                  List.map lift_stmt arg4, annot)
  | Concrete.While (arg1, arg2, arg3, annot) ->
    Abstract.While (lift_expr arg1, List.map lift_stmt arg2, List.map lift_stmt arg3, annot)
  | Concrete.If (arg1, arg2, arg3, annot) ->
    Abstract.If (lift_expr arg1, List.map lift_stmt arg2, List.map lift_stmt arg3, annot)
  | Concrete.Expr (arg1, annot) ->
    Abstract.Expr (lift_expr arg1, annot)
  | Concrete.Pass (annot) ->
    Abstract.Pass (annot)
  | Concrete.Break (annot) ->
    Abstract.Break (annot)
  | Concrete.Continue (annot) ->
    Abstract.Continue (annot)

and lift_expr e =
  match e with
  | Concrete.BoolOp (arg1, arg2, annot) ->
    Abstract.BoolOp (lift_boolop arg1, List.map lift_expr arg2, annot)
  | Concrete.BinOp (arg1, arg2, arg3, annot) ->
    Abstract.BinOp (lift_expr arg1, lift_operator arg2, lift_expr arg3, annot)
  | Concrete.UnaryOp (arg1, arg2, annot) ->
    Abstract.UnaryOp (lift_unaryop arg1, lift_expr arg2, annot)
  | Concrete.IfExp (arg1, arg2, arg3, annot) ->
    Abstract.IfExp (lift_expr arg1, lift_expr arg2, lift_expr arg3, annot)
  | Concrete.Compare (arg1, arg2, arg3, annot) ->
    Abstract.Compare (lift_expr arg1, List.map lift_cmpop arg2,
                      List.map lift_expr arg3, annot)
  | Concrete.Call (arg1, arg2, arg3, arg4, arg5, annot) ->
    Abstract.Call (lift_expr arg1, List.map lift_expr arg2, List.map lift_keyword arg3,
                   lift_option lift_expr arg4, lift_option lift_expr arg5, annot)
  | Concrete.Num (arg1, annot) ->
    Abstract.Num (lift_number arg1, annot)
  | Concrete.Str (_, annot) ->
    Abstract.Str (annot)
  | Concrete.Bool (arg1, annot) ->
    Abstract.Bool (arg1, annot)
  | Concrete.Subscript (arg1, arg2, arg3, annot) ->
    Abstract.Subscript (lift_expr arg1, lift_slice arg2, lift_expr_context arg3, annot)
  | Concrete.Name (arg1, arg2, annot) ->
    Abstract.Name (arg1, lift_expr_context arg2, annot)
  | Concrete.List (arg1, arg2, annot) ->
    Abstract.List (List.map lift_expr arg1, lift_expr_context arg2, annot)
  | Concrete.Tuple (arg1, arg2, annot) ->
    Abstract.Tuple (List.map lift_expr arg1, lift_expr_context arg2, annot)

and lift_expr_context c =
  match c with
  | Concrete.Load -> Abstract.Load
  | Concrete.Store -> Abstract.Store
  | Concrete.Del -> Abstract.Del
  | Concrete.AugLoad -> Abstract.AugLoad
  | Concrete.AugStore -> Abstract.AugStore
  | Concrete.Param -> Abstract.Param

and lift_slice s =
  match s with
  | Concrete.Slice (arg1, arg2, arg3) ->
    Abstract.Slice (lift_option lift_expr arg1, lift_option lift_expr arg2,
                    lift_option lift_expr arg3)
  | Concrete.Index (arg1) ->
    Abstract.Index (lift_expr arg1)

and lift_boolop b =
  match b with
  | Concrete.And -> Abstract.And
  | Concrete.Or -> Abstract.Or

and lift_operator o =
  match o with
  | Concrete.Add -> Abstract.Add
  | Concrete.Sub -> Abstract.Sub
  | Concrete.Mult -> Abstract.Mult
  | Concrete.Div -> Abstract.Div
  | Concrete.Mod -> Abstract.Mod
  | Concrete.Pow -> Abstract.Pow

and lift_unaryop o =
  match o with
  | Concrete.Not -> Abstract.Not
  | Concrete.UAdd -> Abstract.UAdd
  | Concrete.USub -> Abstract.USub

and lift_cmpop c =
  match c with
  | Concrete.Eq -> Abstract.Eq
  | Concrete.NotEq -> Abstract.NotEq
  | Concrete.Lt -> Abstract.Lt
  | Concrete.LtE -> Abstract.LtE
  | Concrete.Gt -> Abstract.Gt
  | Concrete.GtE -> Abstract.GtE
  | Concrete.In -> Abstract.In
  | Concrete.NotIn -> Abstract.NotIn

and lift_arguments a =
  match a with
  | (arg1, arg2, arg3, arg4) ->
    (List.map lift_expr arg1, lift_option (fun x -> x) arg2,
     lift_option (fun x -> x) arg3, List.map lift_expr arg4)

and lift_keyword k =
  match k with
  | (arg1, arg2) ->
    (arg1, lift_expr arg2)

and lift_number n =
  match n with
  | Concrete.Int arg1
  | Concrete.LongInt arg1 ->
    if arg1 > 0 then
      Abstract.Int(Abstract.Pos)
    else if arg1 < 0 then
      Abstract.Int(Abstract.Neg)
    else
      Abstract.Int(Abstract.Zero)
  | Concrete.Float arg1 ->
    if arg1 > 0.0 then
      Abstract.Float(Abstract.Pos)
    else if arg1 < 0.0 then
      Abstract.Float(Abstract.Neg)
    else
      Abstract.Float(Abstract.Zero)
