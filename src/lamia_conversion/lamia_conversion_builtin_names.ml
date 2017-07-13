open Lamia_ast;;

(* This file holds names of lamia variables that can be expected to always
   be in scope. Some (like the python scope) may be shadowed at some points,
   but will always exist *)

(* Name of our local python scope variable *)
let python_scope = Memory_variable("scope");;
(* Function to lookup a variable in the current scope, searching parent
   scopes as necessary *)
let get_from_scope = Value_variable("get_from_scope");;
(* Function to lookup a variable in the parent's scope; called by
   get_from_scope *)
let get_from_parent_scope = Value_variable("get_from_parent_scope");;

(* Builtin python functions *)
let int_add = Memory_variable("builtin_int_add");;
