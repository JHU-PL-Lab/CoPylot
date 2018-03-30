open Batteries;;
open Unique_name_ctx;;
open Python2_ast_types;;
open Python2_simplified_ast;;

type annot = Python2_ast.Pos.t;;

module Simplification_monad:
sig
  type 'a t

  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val emit: annotated_stmt t list -> unit t
  val listen: 'a t -> ('a * annotated_stmt list) t
  val sequence: ('a t) list -> ('a list) t

  val run: name_context -> annot -> 'a t -> 'a * annotated_stmt list

  val local_annot: annot -> 'a t -> 'a t
  val fresh_name: unit -> string t

  val empty: unit t

  (* Smart Constructors *)
  val s_Assign: identifier * annotated_expr -> annotated_stmt t
  val s_Return: annotated_expr -> annotated_stmt t
  val s_While: identifier * annotated_stmt list -> annotated_stmt t
  val s_If: annotated_expr * annotated_stmt list * annotated_stmt list -> annotated_stmt t
  val s_Raise: annotated_expr -> annotated_stmt t
  val s_TryExcept: annotated_stmt list * identifier * annotated_stmt list -> annotated_stmt t
  val s_Pass: annotated_stmt t
  val s_Break: annotated_stmt t
  val s_Continue: annotated_stmt t
  val s_Expr: annotated_expr -> annotated_stmt t

  val s_UnaryOp: unaryop * annotated_expr -> annotated_expr t
  val s_Binop: annotated_expr * binop * annotated_expr -> annotated_expr t
  val s_Call: annotated_expr * annotated_expr list -> annotated_expr t
  val s_Attribute: annotated_expr * string -> annotated_expr t
  val s_List: annotated_expr list -> annotated_expr t
  val s_Tuple: annotated_expr list -> annotated_expr t
  val s_Num: number -> annotated_expr t
  val s_Str: string -> annotated_expr t
  val s_Bool: bool -> annotated_expr t
  val s_Name: identifier -> annotated_expr t
  val s_Builtin: builtin -> annotated_expr t
  val s_FunctionVal: identifier list * annotated_stmt list -> annotated_expr t
end =
struct
  (* TODO: Change to not use a list for performance reasons *)
  type 'a t = name_context -> annot -> 'a * annotated_stmt list;;

  let return x = fun _ _ -> (x, []);;

  let bind x f =
    fun ctx annot ->
      let a, stmts = x ctx annot in
      let b, new_stmts = f a ctx annot in
      b, stmts @ new_stmts
  ;;

  let listen x =
    fun ctx annot ->
      x ctx annot, []
  ;;

  let sequence lst =
    let rec execute lst =
      match lst with
      | [] -> return []
      | hd::tl ->
        let%bind hd_result = hd in
        let%bind tl_result = execute tl in
        return @@ hd_result::tl_result
    in
    execute lst
  ;;

  let run ctx annot m = m ctx annot;;

  let emit stmts =
    fun ctx annot ->
      (), List.map (fun x -> fst @@ run ctx annot x) stmts
  ;;

  let local_annot annot m =
    fun ctx _ ->
      m ctx annot
  ;;

  let fresh_name () =
    fun ctx annot ->
      gen_unique_name ctx annot, []
  ;;

  let empty = return ();;

  (* Smart Constructors *)
  let s_Assign (arg1, arg2) =
    fun _ annot ->
      annotate annot @@ Assign(arg1, arg2), []

  let s_Return arg1 =
    fun _ annot ->
      annotate annot @@ Return(arg1), []

  let s_While (arg1, arg2) =
    fun _ annot ->
      annotate annot @@ While(arg1, arg2), []

  let s_If (arg1, arg2, arg3) =
    fun _ annot ->
      annotate annot @@ If(arg1, arg2, arg3), []

  let s_Raise arg1 =
    fun _ annot ->
      annotate annot @@ Raise(arg1), []

  let s_TryExcept (arg1, arg2, arg3) =
    fun _ annot ->
      annotate annot @@ TryExcept(arg1, arg2, arg3), []

  let s_Pass =
    fun _ annot ->
      annotate annot @@ Pass, []

  let s_Break =
    fun _ annot ->
      annotate annot @@ Break, []

  let s_Continue =
    fun _ annot ->
      annotate annot @@ Continue, []

  let s_Expr arg1 =
    fun _ annot ->
      annotate annot @@ Expr(arg1), []

  let s_UnaryOp (arg1, arg2) =
    fun _ annot ->
      annotate annot @@ UnaryOp(arg1, arg2), []

  let s_Binop (arg1, arg2, arg3) =
    fun _ annot ->
      annotate annot @@ Binop(arg1, arg2, arg3), []

  let s_Call (arg1, arg2) =
    fun _ annot ->
      annotate annot @@ Call(arg1, arg2), []

  let s_Attribute (arg1, arg2) =
    fun _ annot ->
      annotate annot @@ Attribute(arg1, arg2), []

  let s_List arg1 =
    fun _ annot ->
      annotate annot @@ List(arg1), []

  let s_Tuple arg1 =
    fun _ annot ->
      annotate annot @@ Tuple(arg1), []

  let s_Num arg1 =
    fun _ annot ->
      annotate annot @@ Num(arg1), []

  let s_Str arg1 =
    fun _ annot ->
      annotate annot @@ Str(arg1), []

  let s_Bool arg1 =
    fun _ annot ->
      annotate annot @@ Bool(arg1), []

  let s_Name arg1 =
    fun _ annot ->
      annotate annot @@ Name(arg1), []

  let s_Builtin arg1 =
    fun _ annot ->
      annotate annot @@ Builtin(arg1), []

  let s_FunctionVal (arg1, arg2) =
    fun _ annot ->
      annotate annot @@ FunctionVal(arg1, arg2), []
end

type 'a m = 'a Simplification_monad.t;;
