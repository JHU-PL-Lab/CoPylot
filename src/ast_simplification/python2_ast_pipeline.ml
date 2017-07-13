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

let parse_to_simplified
    (prog: string)
    (starting_name: int)
    (short_names: bool)
  : 'a Python2_simplified_ast.modl =
  let module Simplify =  Python2_ast_simplifier in
  let prefix = if short_names then "$simp" else "$simplified_unique_name_" in
  let concrete = parse_to_concrete prog in
  let ctx =
    Python2_simplification_ctx.create_new_simplification_ctx starting_name prefix
  in
  Simplify.simplify_modl ctx concrete
;;

let parse_to_normalized
    (prog: string)
    (starting_name: int)
    (short_names: bool)
  : Python2_normalized_ast.modl =
  let module Normalize = Python2_ast_normalizer in
  let prefix = if short_names then "$norm" else "$normalized_unique_name_" in
  let simplified = parse_to_simplified prog 0 short_names in
  let ctx =
    Python2_simplification_ctx.create_new_simplification_ctx starting_name prefix
  in
  Normalize.normalize_modl ctx simplified
;;
