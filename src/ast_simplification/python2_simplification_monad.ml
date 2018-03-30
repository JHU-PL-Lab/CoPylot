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
  val emit: stmt list -> unit t
  val listen: 'a t -> ('a * annotated_stmt list) t
  val sequence: ('a t) list -> ('a list) t

  val run: name_context -> annot -> 'a t -> 'a * annotated_stmt list

  val local_annot: annot -> 'a t -> 'a t
  val fresh_name: unit -> string t

  val empty: unit t

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
    fun _ annot ->
      (), List.map (fun s -> annotate annot s) stmts
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

type 'a m = 'a Simplification_monad.t;;
