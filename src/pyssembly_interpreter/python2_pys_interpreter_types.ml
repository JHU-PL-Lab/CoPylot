open Batteries;;
open Python2_normalized_ast;;

type label =
  | Uid of uid
  | End
[@@deriving eq, ord, show]
;;

(* A list of statements to be executed, all in the same scope.
   Correponds to "S" in the grammar *)
module Body : sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val pp: Format.formatter -> t -> unit

  val create: annotated_stmt list -> t

  (* Return the uid of the first statement in the body. Not necessarily
     the smallest one. *)
  val get_first_uid: t -> uid

  (* Retrieve the statement with the given uid *)
  val get_stmt: t -> uid -> annotated_stmt

  (* Return the uid of the next statement in the list, or End if none.
     WARNING: this is not necessarily the next statement to be executed if
     the first statement is a Goto or GotoIfNot statement.*)
  val get_next_label: t -> uid -> label
end =
struct
  module Uid_ord =
  struct
    type t = uid
    let compare = compare_uid
  end
  module Uid_map = Map.Make(Uid_ord);;
  type order_map = label Uid_map.t;;

  type t = { stmts: annotated_stmt list; order: order_map};;

  let compare t1 t2 = List.compare compare_annotated_stmt t1.stmts t2.stmts;;
  let equal t1 t2 = List.eq equal_annotated_stmt t1.stmts t2.stmts;;
  let pp fmt t1 = List.iter (pp_annotated_stmt fmt) t1.stmts;; (* TODO: Not sure this works *)

  let get_first_uid b = let fst = List.hd b.stmts in fst.uid;;

  let get_stmt (b : t) (u : uid) = List.find (fun s -> (s.uid = u)) b.stmts;;

  let create (stmts : annotated_stmt list) =
    let labels = List.map (fun s -> Uid(s.uid)) stmts in
    let lexical_map =
      List.fold_left2
        (fun (map : order_map) (s : annotated_stmt) (lab : label) : order_map ->
           Uid_map.add s.uid lab map)
        Uid_map.empty
        stmts
        ((List.tl labels) @ [End])
    in
    {stmts = stmts; order = lexical_map}
  ;;

  let get_next_label (b : t) (u : uid) = Uid_map.find u b.order;;
end
;;

(* A single stack frame, composed of the code to execute and the uid of the
   stmt to execute next. Corresponds to "t" in the grammar. *)
module Stack_frame =
struct
  type t = { curr_uid: uid; body: Body.t }
  [@@deriving eq, ord, show]
  ;;

  (* Get the statement pointed to by the current uid, which should be the
     next statement we execute *)
  let active_stmt (frame : t) = Body.get_stmt frame.body frame.curr_uid;;

  (* Change the currently active uid *)
  let update_uid (frame : t) (new_uid : uid) : t =
    { curr_uid = new_uid; body = frame.body };;

  (* Create a new stack frame from a statement body *)
  let create (body : Body.t) : t =
    let start_uid = Body.get_first_uid body in
    { curr_uid = start_uid; body = body}
  ;;

end
;;

(* Exactly what it says on the tin. Corresponds to "T" in the grammar. *)
module Stack =
struct
  type t = Stack_frame.t list
  [@@deriving eq, ord, show]
  ;;
end
;;

(* A location in memory. Corresponds to "m" in the grammar. *)
type memloc = Memloc of int
[@@deriving eq, ord, show]
;;

(* Etas serve to identify various binding sets. *)
type eta = Eta of int
[@@deriving eq, ord, show]
;;

(* The possible values in the program. Corresponds to "v" in the grammar. *)
type value =
  | Num of number
  | Str of str
  | Bool of bool
  | Builtin of builtin
  | Function of eta * identifier list (* arglist *) * Body.t
[@@deriving eq, ord, show]
;;

(* We represent our various sets of maps as different types of hashtable. *)

(* A variable binding (x,m) indicates that the variable x is bound to
   memory location m. These correspond to "B" in the grammar *)
module Bindings : sig
  type t

  (* Add a binding of the input memloc to the given value, overwriting
     the previous entry if one exists *)
  val update_binding: t -> identifier -> memloc -> t

  (* Get the memory location bound to the current variable, if any *)
  val get_memloc: t -> identifier -> memloc

  val empty: t
  val singleton: identifier -> memloc -> t
end =
struct
  module Var_ord =
  struct
    type t = identifier
    let compare = compare_identifier
  end
  module Var_map = Map.Make(Var_ord);;

  type t = memloc Var_map.t;;

  let update_binding prev id mem = Var_map.add id mem prev;;

  let get_memloc map id = Var_map.find id map;;

  let empty = Var_map.empty;;

  let singleton id mem = Var_map.singleton id mem;;
end
;;

(* A heap binding (m,v) indicates that the memory location m contains the
   value v. These correspond to "H" in the grammar *)
module Heap: sig
  type t

  (* Return a memloc which does not yet appear in the heap *)
  val get_new_memloc: t -> memloc

  (* Add a binding of the input memloc to the given value, overwriting
     the previous entry if one exists *)
  val update_binding: t -> memloc -> value -> t

  (* Get the value bound to the given location in memory, if there is one. *)
  val get_value: t -> memloc -> value

  val empty: t
  val singleton: memloc -> value -> t
end =
struct
  module Memloc_ord =
  struct
    type t = memloc
    let compare = compare_memloc
  end
  module Memloc_map = Map.Make(Memloc_ord);;

  (* The maxval entry tracks the maximum value of the keys in the map, so we
     can generate new ones easily *)
  type t = { map: value Memloc_map.t; maxval: int } ;;

  let get_new_memloc heap = Memloc(heap.maxval + 1);;

  let update_binding heap mem value =
    let new_map = Memloc_map.add mem value heap.map in
    let Memloc(mem_val) = mem in
    let new_max = max heap.maxval mem_val in
    { map = new_map; maxval = new_max };;

  let get_value heap mem = Memloc_map.find mem heap.map;;

  let empty = { map = Memloc_map.empty; maxval = 0 };;

  let singleton mem value =
    let map = Memloc_map.singleton mem value in
    let Memloc(memval) = mem in
    { map = map; maxval = memval};;
end
;;

(* An environment is the map of etas to scopes, so we know which ones we are
   allowed to search for variables in. Corresponds to "E" in the grammar. *)
module Environment : sig
  type t

  val update_binding: t -> eta -> Bindings.t -> t

  val get_binding: t-> eta -> Bindings.t

  val empty: t
  val singleton: eta -> Bindings.t -> t
end =
struct
  module Eta_ord =
  struct
    type t = eta
    let compare = compare_eta
  end
  module Eta_map = Map.Make(Eta_ord);;

  type t = Bindings.t Eta_map.t;;

  let update_binding prev eta b = Eta_map.add eta b prev;;

  let get_binding map eta = Eta_map.find eta map;;

  let empty = Eta_map.empty;;

  let singleton eta b = Eta_map.singleton eta b;;
end;;

(* The parents map holds the relationship between etas; children search their
   parent's corresponding binding if they encounter an unbound variable. *)
module Parents : sig
  type t

  (* Register the first input eta as a child of the second *)
  val add_parent: t -> eta -> eta -> t

  (* Retrieve the parent of the input eta, if any *)
  val get_parent: t -> eta -> eta

  val empty: t
  val singleton: eta -> eta -> t
end =
struct
  module Eta_ord =
  struct
    type t = eta
    let compare = compare_eta
  end
  module Eta_map = Map.Make(Eta_ord);;

  type t = eta Eta_map.t;;

  let add_parent prev child parent = Eta_map.add child parent prev;;

  let get_parent map child = Eta_map.find child map;;

  let empty = Eta_map.empty;;

  let singleton child parent = Eta_map.singleton child parent;;
end;;
