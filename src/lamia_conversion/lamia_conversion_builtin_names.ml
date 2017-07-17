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

let builtin_true = Value_variable ("*True");;
let builtin_false = Value_variable("*False");;
let builtin_none = Value_variable("*None");;

(* Builtin global python functions *)
(* Technically these are constructor calls *)
(* let builtin_type = Memory_variable("builtin_type");; *)
let builtin_bool = Memory_variable("*bool");;
let builtin_slice = Memory_variable("*slice");;
let builtin_ValueError = Memory_variable("*ValueError");;
let builtin_AttributeError = Memory_variable("*AttributeError");;
let builtin_TypeError = Memory_variable("*TypeError");;

(* Builtin python methods *)
let int_add = Memory_variable("*int_add");;

let method_call = Memory_variable("*method_call");;
