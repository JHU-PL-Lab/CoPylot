open OUnit2;;
open Batteries;;
open Jhupllib;;

open Python2_ast_pipeline;;

open Lamia_converter;;
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
      let prog = file_to_string ("./test/regression/tests/"^filename^".test") in
      let expected = file_to_string ("./test/regression/expected/"^filename^".lamia") in

      let actual = parse_to_normalized_safe prog 1 true in
      let ctx = Unique_name_ctx.create_new_name_ctx 0 "lamia$" in
      let lamia_prog = convert_module ctx actual in
      let lamia_uid_prog, _ = annot_to_uid lamia_prog in
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

let tests =
  "test_lamia_converter">:::
  [
    gen_module_test "int_add_test"
  ]
