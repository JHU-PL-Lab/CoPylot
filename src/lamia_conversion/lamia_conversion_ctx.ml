open Batteries;;

module Counter =
struct
  type t = int

  let hash x = x
  let equal (x1 : int) (x2 : int) = (x1 = x2)
end
;;

module Counter_hashtbl = Hashtbl.Make(Counter);;

type conversion_context = {
  mutable uid_counter: int;
  mutable name_counter: int;
  name_prefix: string;
  uid_map: Python2_ast.Pos.t Counter_hashtbl.t;
  name_map: Python2_ast.Pos.t Counter_hashtbl.t;
}
;;

let create_new_conversion_ctx starting_uid starting_name name_prefix =
  { uid_counter = starting_uid; name_counter = starting_name;
    name_prefix = name_prefix;
    uid_map = Counter_hashtbl.create 10; name_map = Counter_hashtbl.create 10; }
;;

let get_next_uid ctx annot =
  let count = ctx.uid_counter in
  ctx.uid_counter <- count + 1;
  Counter_hashtbl.add ctx.uid_map count annot;
  count
;;

let gen_unique_name ctx annot =
  let count = ctx.name_counter in
  ctx.name_counter <- count + 1;
  Counter_hashtbl.add ctx.name_map count annot;
  ctx.name_prefix ^ string_of_int count
;;

let get_annotation_from_uid ctx u =
  Counter_hashtbl.find ctx.uid_map u
;;

let get_annotation_from_name ctx u =
  Counter_hashtbl.find ctx.name_map u
;;
