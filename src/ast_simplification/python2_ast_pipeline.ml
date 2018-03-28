open Python2_ast_types;;

(* Functions to parse an input program to any desired point in the ast pipeline *)
let parse_to_base (prog: string) : annot Python2_ast.modl =
  Python2_parser.parse_from_string (prog ^ "\n")
;;

let parse_to_renamed (prog: string) : 'a Python2_ast.modl =
  let module Rename = Python2_ast_renamer in
  let base = parse_to_base prog in
  Rename.rename_modl Rename.Id_map.empty [] base
;;

let parse_to_augmented (prog: string) : annot Python2_augmented_ast.modl =
  let renamed = parse_to_renamed prog in
  Python2_ast_augmentor.augment_modl renamed
;;

let parse_to_simplified
    (prog: string)
    (starting_name: int)
    (short_names: bool)
  : annot Python2_simplified_ast.modl =
  let module Simplify =  Python2_ast_simplifier in
  let prefix = if short_names then "$simp" else "$simplified_unique_name_" in
  let augmented = parse_to_augmented prog in
  let ctx =
    Unique_name_ctx.create_new_name_ctx starting_name prefix
  in
  Simplify.simplify_modl ctx augmented
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
    Unique_name_ctx.create_new_name_ctx starting_name prefix
  in
  Normalize.normalize_modl ctx simplified
;;
