(* Functions to parse an input program to any desired point in the ast pipeline *)
let parse_to_base (prog: string) : 'a Python2_ast.modl =
  Python2_parser.parse_from_string (prog ^ "\n")
;;

let parse_to_renamed (prog: string) : 'a Python2_ast.modl =
  let module Rename = Python2_rename_ast in
  let base = parse_to_base prog in
  Rename.rename_modl Rename.Id_map.empty [] base
;;

let parse_to_concrete (prog: string) : 'a Python2_concrete_ast.modl =
  let renamed = parse_to_renamed prog in
  Python2_ast_generalizer.generalize_modl renamed
;;

let parse_to_simplified (prog: string) (short_names: bool)
  : 'a Python2_simplified_ast.modl =
  let module Simplify =  Python2_ast_simplifier in
  let concrete = parse_to_concrete prog in
  Simplify.toggle_short_names short_names;
  Simplify.simplify_modl concrete
;;

let parse_to_normalized
    (prog: string)
    (short_names: bool)
    (include_builtin_defs : bool)
    (starting_uid: int)
  : Python2_normalized_ast.modl =
  let module Normalize = Python2_ast_normalizer in
  let simplified = parse_to_simplified prog short_names in
  let ctx = Uid_generation.create_new_uid_context starting_uid in
  Normalize.toggle_short_names short_names;
  if include_builtin_defs then
    Normalize.normalize_modl ctx simplified
  else
    Normalize.normalize_modl_simple ctx simplified
;;

let parse_to_abstract
    (prog: string)
    (short_names: bool)
    (include_builtin_defs : bool)
    (starting_uid: int)
  :  Python2_abstract_ast.modl =
  let normalized = parse_to_normalized prog short_names include_builtin_defs starting_uid in
  Python2_ast_lifter.lift_modl normalized
;;
