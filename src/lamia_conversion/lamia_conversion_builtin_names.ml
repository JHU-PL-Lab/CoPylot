open Lamia_ast;;

(* Name of our local python scope variable *)
(* TODO: Define these in preamble  *)
let python_scope = Memory_variable("scope");;
let parent_scope = Memory_variable("parent_scope");;
(* At top level, throws an exception *)
let get_from_scope = Value_variable("get_from_scope");;
let get_from_parent_scope = Value_variable("get_from_parent_scope");;
