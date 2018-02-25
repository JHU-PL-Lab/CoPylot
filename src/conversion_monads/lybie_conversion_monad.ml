open Generic_conversion_monad;;

module Lybie_Lang =
struct
  type directive = annot Lybie_ast.directive
  type statement = annot Lybie_ast.statement
  let annotate annot d = Lybie_ast.Statement(annot, d)
end

module Lybie_monad = Conversion_monad(Lybie_Lang);;

type 'a m = 'a Lybie_monad.t;;

(* Useful functions to make use of this *)
open Lamia_ast_types;;
open Lybie_monad;;

let fresh_value_var () : value_variable m =
  let%bind name = fresh_name () in
  return @@ Value_variable(name)
;;

let fresh_memory_var () : memory_variable m =
  let%bind name = fresh_name () in
  return @@ Memory_variable("&" ^ name)
;;
