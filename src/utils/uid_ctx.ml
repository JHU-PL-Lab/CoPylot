open Batteries;;
open Counter_hashtbl;;

module Annot_ord =
struct
  type t = Python2_ast.Pos.t

  let hash x =
    let lex_pos = Python2_ast.Pos.to_pos x in
    lex_pos.Lexing.pos_cnum

  let equal x1 x2 =
    let lex_pos1 = Python2_ast.Pos.to_pos x1 in
    let lex_pos2 = Python2_ast.Pos.to_pos x2 in
    lex_pos1.Lexing.pos_cnum = lex_pos2.Lexing.pos_cnum
end
;;

module Annot_hashtbl = Hashtbl.Make(Annot_ord);;

type uid_context = {
  mutable uid_counter: int;
  uid_map: Python2_ast.Pos.t Counter_hashtbl.t;
  annot_map: int Annot_hashtbl.t;
}
;;

let create_new_uid_ctx starting_uid =
  {
    uid_counter = starting_uid;
    uid_map = Counter_hashtbl.create 10;
    annot_map = Annot_hashtbl.create 10;
  }
;;

let get_next_uid ctx annot =
  let count = ctx.uid_counter in
  ctx.uid_counter <- count + 1;
  Counter_hashtbl.add ctx.uid_map count annot;
  Annot_hashtbl.add ctx.annot_map annot count;
  count
;;

let get_annotation_from_uid ctx u =
  Counter_hashtbl.find ctx.uid_map u
;;

let get_uids_from_annot ctx a =
  Annot_hashtbl.find_all ctx.annot_map a
;;
