open Batteries;;
open Unique_name_ctx;;

type annot = Python2_ast.Pos.t;;

module type Language =
sig
  type statement
  type directive
  val annotate: annot -> directive -> statement
end;;

module Conversion_monad(Lang : Language):
sig
  type 'a t

  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val emit: Lang.directive list -> unit t
  val listen: 'a t -> ('a * Lang.statement list) t
  val sequence: ('a t) list -> ('a list) t

  val run: name_context -> annot -> 'a t -> 'a * Lang.statement list

  val local_annot: annot -> 'a t -> 'a t
  val fresh_name: unit -> string t

  val empty: unit t
end =
struct
  (* TODO: Change to not use a list for performance reasons *)
  type 'a t = name_context -> annot -> 'a * Lang.statement list;;

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

  let emit directives =
    fun _ annot ->
      let stmts = List.map (Lang.annotate annot) directives in
      (), stmts
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
end
