open Batteries;;

module Uid =
struct
  type t = int

  let hash x = x
  let equal (x1 : int) (x2 : int) = (x1 = x2)
end
;;

module Uid_hashtbl = Hashtbl.Make(Uid);;

type uid_context = {
  mutable uid_counter: int;
  annotation_map: Python2_ast.Pos.t Uid_hashtbl.t
}

let create_new_uid_context () =
  { uid_counter = 0; annotation_map = Uid_hashtbl.create 10}

let get_next_uid ctx annot =
  let count = ctx.uid_counter in
  ctx.uid_counter <- count + 1;
  Uid_hashtbl.add ctx.annotation_map count annot;
  count;;

let get_annotation_from_uid ctx u =
  Uid_hashtbl.find ctx.annotation_map u;;
