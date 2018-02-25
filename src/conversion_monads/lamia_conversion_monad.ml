open Generic_conversion_monad;;
(* open Lamia_ast;; *)

module Lamia_Lang =
struct
  type directive = annot Lamia_ast.directive
  type statement = annot Lamia_ast.statement
  let annotate annot d = Lamia_ast.Statement(annot, d)
end

module Lamia_monad = Conversion_monad(Lamia_Lang);;

type 'a m = 'a Lamia_monad.t;;

(* Useful functions to make use of this *)
open Lamia_ast_types;;
open Lamia_monad;;

let fresh_value_var () : value_variable m =
  let%bind name = fresh_name () in
  return @@ Value_variable(name)
;;

let fresh_memory_var () : memory_variable m =
  let%bind name = fresh_name () in
  return @@ Memory_variable("&" ^ name)
;;
