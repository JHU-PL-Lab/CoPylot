(* open Batteries;;
open Lamia_ast;; *)

(* This file contains all definitions of python builtins, global helper
   functions like *getcall, etc *)

let annot = Python2_ast.Pos.of_pos Lexing.dummy_pos;;

(* TODO: Define function values *)
(* TODO: Store in global memlocs *)
