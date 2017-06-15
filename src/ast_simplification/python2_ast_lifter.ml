open Python2_ast_types;;
module Abstract = Python2_abstract_ast;;
module Concrete = Python2_normalized_ast;;

(* Lets us lift optional types easily *)
let lift_option func opt =
  match opt with
  | None -> None
  | Some(x) -> Some(func x)
;;

let rec lift_modl m =
  match m with
  | Concrete.Module (statements, uid) ->
    Abstract.Module (List.map lift_stmt statements, uid)

and lift_stmt { uid = u; exception_target = ex; multi = m; body = old_body } =
  let new_body =
    match old_body with
    | Concrete.Assign (id, value) ->
      Abstract.Assign (id, lift_expr value)
    | Concrete.Return (id) ->
      Abstract.Return (id)
    | Concrete.Print (id, args, b) ->
      Abstract.Print (id, args, b)
    | Concrete.Raise (id) ->
      Abstract.Raise (id)
    | Concrete.Catch (id) ->
      Abstract.Catch (id)
    | Concrete.Pass ->
      Abstract.Pass
    | Concrete.Goto (dest) ->
      Abstract.Goto (dest)
    | Concrete.GotoIfNot (id, dest) ->
      Abstract.GotoIfNot (id, dest)
    | Concrete.NameStmt (id) ->
      Abstract.NameStmt (id)
  in
  { uid = u; exception_target = ex; multi = m; body = new_body }

and lift_expr { uid = u; exception_target = ex; multi = m; body = old_body } =
  let new_body =
    match old_body with
    | Concrete.Call (func, args) ->
      Abstract.Call (func, args)
    | Concrete.Attribute (obj, attr) ->
      Abstract.Attribute (obj, attr)
    | Concrete.Name (id) ->
      Abstract.Name (id)
    | Concrete.List (elts) ->
      Abstract.List (elts)
    | Concrete.Tuple (elts) ->
      Abstract.Tuple (elts)
    | Concrete.Literal (l) ->
      Abstract.Literal(lift_literal l)
  in
  { uid = u; exception_target = ex; multi = m; body = new_body }

and lift_literal l =
  match l with
  | Concrete.Num (n) ->
    Abstract.Num (lift_number n)
  | Concrete.Str (Concrete.StringLiteral(str)) ->
    Abstract.Str (Abstract.StringLiteral(str))
  | Concrete.Bool (b) ->
    Abstract.Bool (b)
  | Concrete.Builtin (b) ->
    Abstract.Builtin (lift_builtin b)
  | Concrete.FunctionVal (args, body) ->
    Abstract.FunctionVal (args, List.map lift_stmt body)

and lift_number n =
  match n with
  | Concrete.Int arg1 ->
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

and lift_builtin b =
  match b with
  | Concrete.Builtin_slice -> Abstract.Builtin_slice
  | Concrete.Builtin_bool -> Abstract.Builtin_bool
  | Concrete.Builtin_type -> Abstract.Builtin_type
