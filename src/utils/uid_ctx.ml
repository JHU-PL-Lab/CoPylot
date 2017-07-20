open Batteries;;
open Counter_hashtbl;;

type uid_context = {
  mutable uid_counter: int;
  uid_map: Python2_ast.Pos.t Counter_hashtbl.t;
}
;;

let create_new_uid_ctx starting_uid =
  { uid_counter = starting_uid;
    uid_map = Counter_hashtbl.create 10; }
;;

let get_next_uid ctx annot =
  let count = ctx.uid_counter in
  ctx.uid_counter <- count + 1;
  Counter_hashtbl.add ctx.uid_map count annot;
  count
;;

let get_annotation_from_uid ctx u =
  Counter_hashtbl.find ctx.uid_map u
;;
