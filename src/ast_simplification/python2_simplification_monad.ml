open Batteries;;
open Unique_name_ctx;;
open Python2_ast_types;;
open Python2_simplified_ast;;

type annot = Python2_ast.Pos.t;;

module Simplification_monad:
sig
  type 'a t
  type statement = annot Python2_simplified_ast.stmt
  type expression = annot Python2_simplified_ast.expr

  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val emit: statement t list -> unit t
  val listen: 'a t -> ('a * statement list) t
  val sequence: ('a t) list -> ('a list) t

  val run: name_context -> annot -> 'a t -> 'a * statement list

  val local_annot: annot -> 'a t -> 'a t
  val fresh_name: unit -> string t

  val empty: unit t

  (* Smart Constructors *)
  val s_Assign: identifier -> expression -> statement t
  val s_Return: expression -> statement t
  val s_While: identifier -> statement list -> statement t
  val s_If: expression -> statement list -> statement list -> statement t
  val s_Raise: expression -> statement t
  val s_TryExcept: statement list -> identifier -> statement list -> statement t
  val s_Pass: statement t
  val s_Break: statement t
  val s_Continue: statement t
  val s_Expr: expression -> statement t

  val s_UnaryOp: unaryop -> expression -> expression t
  val s_Binop: expression -> binop -> expression -> expression t
  val s_Call: expression -> expression list -> expression t
  val s_Attribute: expression -> string -> expression t
  val s_List: expression list  -> expression t
  val s_Tuple: expression list  -> expression t
  val s_Num: number -> expression t
  val s_Str: string -> expression t
  val s_Bool: bool -> expression t
  val s_Name: identifier -> expression t
  val s_Builtin: builtin -> expression t
  val s_FunctionVal: identifier list -> statement list -> expression t
end =
struct
  type statement = annot Python2_simplified_ast.stmt;;
  type expression = annot Python2_simplified_ast.expr;;

  (* TODO: Change to not use a list for performance reasons *)
  type 'a t = name_context -> annot -> 'a * statement list;;

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
  let s_Assign arg1 arg2 =
    fun _ annot ->
      Assign(arg1, arg2, annot), []

  let s_Return arg1 =
    fun _ annot ->
      Return(arg1, annot), []

  let s_While arg1 arg2 =
    fun _ annot ->
      While(arg1, arg2, annot), []

  let s_If arg1 arg2 arg3 =
    fun _ annot ->
      If(arg1, arg2, arg3, annot), []

  let s_Raise arg1 =
    fun _ annot ->
      Raise(arg1, annot), []

  let s_TryExcept arg1 arg2 arg3 =
    fun _ annot ->
      TryExcept(arg1, arg2, arg3, annot), []

  let s_Pass =
    fun _ annot ->
      Pass(annot), []

  let s_Break =
    fun _ annot ->
      Break(annot), []

  let s_Continue =
    fun _ annot ->
      Continue(annot), []

  let s_Expr arg1 =
    fun _ annot ->
      Expr(arg1, annot), []

  let s_UnaryOp arg1 arg2 =
    fun _ annot ->
      UnaryOp(arg1, arg2, annot), []

  let s_Binop arg1 arg2 arg3 =
    fun _ annot ->
      Binop(arg1, arg2, arg3, annot), []

  let s_Call arg1 arg2 =
    fun _ annot ->
      Call(arg1, arg2, annot), []

  let s_Attribute arg1 arg2 =
    fun _ annot ->
      Attribute(arg1, arg2, annot), []

  let s_List arg1 =
    fun _ annot ->
      List(arg1, annot), []

  let s_Tuple arg1 =
    fun _ annot ->
      Tuple(arg1, annot), []

  let s_Num arg1 =
    fun _ annot ->
      Num(arg1, annot), []

  let s_Str arg1 =
    fun _ annot ->
      Str(arg1, annot), []

  let s_Bool arg1 =
    fun _ annot ->
      Bool(arg1, annot), []

  let s_Name arg1 =
    fun _ annot ->
      Name(arg1, annot), []

  let s_Builtin arg1 =
    fun _ annot ->
      Builtin(arg1, annot), []

  let s_FunctionVal arg1 arg2 =
    fun _ annot ->
      FunctionVal(arg1, arg2, annot), []
end

type 'a m = 'a Simplification_monad.t;;
