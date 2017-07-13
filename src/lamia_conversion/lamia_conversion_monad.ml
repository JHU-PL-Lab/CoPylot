open Batteries;;
open Lamia_ast;;
open Lamia_conversion_ctx;;

module Conversion_monad :
sig
  type 'a t

  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val emit: directive list -> unit t
  val run: conversion_context -> Python2_ast.Pos.t -> 'a t -> 'a * statement list

  val local_annot: Python2_ast.Pos.t -> 'a t -> 'a t
  val fresh_name: unit -> string t
end =
struct
  type 'a t = conversion_context -> Python2_ast.Pos.t -> 'a * statement list;;

  let return x = fun _ _ -> (x, []);;

  let bind x f =
    fun ctx annot ->
      let a, stmts = x ctx annot in
      let b, new_stmts = f a ctx annot in
      b, stmts @ new_stmts
  ;;

  let run ctx annot m = m ctx annot;;

  let annotate_directive ctx annot d =
    Statement(get_next_uid ctx annot, d)
  ;;

  let emit directives =
    fun ctx annot ->
      let stmts = List.map (annotate_directive ctx annot) directives in
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
end
