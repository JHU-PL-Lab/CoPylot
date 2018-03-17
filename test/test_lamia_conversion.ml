open OUnit2;;
open Batteries;;
open Jhupllib;;

open Python2_ast_pipeline;;

open Lamia_ast_pretty;;

let parse_to_normalized_safe prog short_names =
  try
    parse_to_normalized prog short_names
  with
  | Python2_parser.Parse_error p ->
    let open Lexing in
    assert_failure (Printf.sprintf "Error in line %d, col %d."
                      p.pos_lnum p.pos_cnum)
;;

let lamia_to_string prog = Pp_utils.pp_to_string pp_block_top prog;;

let validate_filename filename =
  (* Filenames can't have newlines (this ensures that $ works right) *)
  if String.contains filename '\n' then false else
  (* String must be alphanumeric or underscores only *)
  let valid_regex = Str.regexp "^[a-zA-z0-9_]+$" in
  Str.string_match valid_regex filename 0
;;

let file_to_string filename =
  let lines = File.lines_of filename in
  let str =
    Enum.fold (fun lines line -> lines ^ "\n" ^ line) "" lines
  in
  String.trim str
;;

let gen_module_test (filename : string) =
  let name = ("conversion_test:"^filename) in
  name >::
  ( fun _ ->
      assert_bool "Bad filename!" @@ validate_filename filename;
      let prog = file_to_string ("./test/regression/tests/"^filename^".test") in
      (* If we can't read from the file, assume this is a new test. *)
      let expected =
        try
          file_to_string ("./test/regression/expected/"^filename^".lamia")
        with
        | Sys_error error ->
          if String.exists error "No such file" then ""
          else raise (Sys_error error)
      in

      let actual = parse_to_normalized_safe prog 1 true in
      let ctx = Unique_name_ctx.create_new_name_ctx 0 "lamia$" in
      let lybie_prog = Lybie_converter.convert_module ctx actual in
      let lamia_prog = Lybie_expander.expand_macros_block ctx lybie_prog in
      let lamia_uid_prog, _ = Lybie_expander.annot_to_uid lamia_prog in
      let lamia_str = String.trim @@ lamia_to_string lamia_uid_prog in

      (* If our output changed, write to a file for inspection *)
      begin
        if not (String.equal expected lamia_str) then
          let outfile = "./test/regression/output/"^filename^".lamia" in
          File.write_lines outfile (Enum.singleton lamia_str)
      end;

      assert_equal ~printer:(fun x -> x) ~cmp:String.equal expected lamia_str;
  )
;;

(* Commented tests are for features which are not yet implemented *)
let tests =
  "test_lamia_converter">:::
  [
      gen_module_test "add_test";
      gen_module_test "and_test";
      gen_module_test "assign_test";
      gen_module_test "assign_test_2";
      gen_module_test "attr_test";
      gen_module_test "augassign_add_test";
      gen_module_test "augassign_sub_test";
      gen_module_test "bool_test";
      (* gen_module_test "break_test"; *)
      gen_module_test "call_test";
      gen_module_test "call_test_2";
      gen_module_test "call_test_3";
      (* gen_module_test "class_def_test"; *)
      (* gen_module_test "continue_test"; *)
      gen_module_test "def_test_1";
      gen_module_test "def_test_2";
      gen_module_test "def_test_3";
      gen_module_test "eq_test";
      gen_module_test "factorial_test";
      (* gen_module_test "float_test"; *)
      gen_module_test "for_test";
      gen_module_test "ge_test";
      gen_module_test "if_test";
      gen_module_test "ifexp_test";
      gen_module_test "int_test";
      gen_module_test "is_test";
      gen_module_test "leq_test";
      gen_module_test "list_test";
      gen_module_test "list_test_empty";
      gen_module_test "list_test_singleton";
      gen_module_test "neg_test";
      gen_module_test "none_test";
      gen_module_test "not_test";
      gen_module_test "or_test";
      gen_module_test "pass_test";
      gen_module_test "pos_test";
      gen_module_test "raise_test";
      gen_module_test "slice_tests";
      gen_module_test "str_test";
      gen_module_test "sub_test";
      gen_module_test "subscript_test";
      gen_module_test "try_test";
      gen_module_test "tuple_test";
      gen_module_test "tuple_test_singleton";
      gen_module_test "var_test";
      gen_module_test "while_test";
      gen_module_test "while_test_2";
  ]
