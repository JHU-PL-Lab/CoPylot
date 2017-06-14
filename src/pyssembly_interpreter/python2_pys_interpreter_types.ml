open Batteries;;
open Python2_normalized_ast;;

type label =
  | Uid of uid
  | End
[@@deriving eq, ord, show]
;;

(* A location in memory. Corresponds to "m" in the grammar. *)
type memloc = Memloc of int
[@@deriving eq, ord, show]
;;

(* Etas serve to identify various binding sets. *)
type eta = Eta of int
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
  val get_next_label: t -> label -> label option
end =
struct
  module Label_ord =
  struct
    type t = label
    let compare = compare_label
  end
  module Label_map = Map.Make(Label_ord);;
  type order_map = label Label_map.t;;

  type t = { stmts: annotated_stmt list; order: order_map};;

  let compare t1 t2 = List.compare compare_annotated_stmt t1.stmts t2.stmts;;
  let equal t1 t2 = List.eq equal_annotated_stmt t1.stmts t2.stmts;;
  let pp fmt t1 = List.iter (pp_annotated_stmt fmt) t1.stmts;; (* TODO: Not sure this works *)

  let get_first_uid b =
    let fst = List.hd b.stmts in fst.uid;;

  let get_stmt (b : t) (u : uid) = List.find (fun s -> (s.uid = u)) b.stmts;;

  let create (stmts : annotated_stmt list) =
    let labels = List.map (fun s -> Uid(s.uid)) stmts in
    let lexical_map =
      List.fold_left2
        (fun (map : order_map) l1 l2 : order_map ->
           Label_map.add l1 l2 map)
        Label_map.empty
        labels
        ((List.tl labels) @ [End])
    in
    {stmts = stmts; order = lexical_map}
  ;;

  let get_next_label (b : t) (l : label) =
    try
      let next = Label_map.find l b.order in
      Some(next)
    with
    | Not_found -> None;;
end
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

(* A single stack frame, composed of the code to execute and the uid of the
   stmt to execute next. Corresponds to "t" in the grammar. *)
module Stack_frame : sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val pp: Format.formatter -> t -> unit

  (* Get the statement pointed to by the current uid, which should be the
     next statement we execute *)
  val active_stmt: t -> annotated_stmt option
  val get_next_label: t -> label

  (* Advance the frame to the specified label *)
  val advance: t -> label -> t

  (* Create a new stack frame from a statement body *)
  val create: Body.t -> t

end =
struct
  type t = { curr_label: label; body: Body.t }
  [@@deriving eq, ord, show]
  ;;
  ignore @@ show;; (* The fact that we need this is definitely a bug *)

  let active_stmt (frame : t) =
    match frame.curr_label with
    | End -> None
    | Uid u -> Some(Body.get_stmt frame.body u);;

  let get_next_label (frame : t) =
    let next = Body.get_next_label frame.body frame.curr_label in
    match next with
    | None -> failwith "Can't move past end of stack frame"
    | Some(l) -> l
;;

  let advance (frame : t) (new_label : label) : t =
    { curr_label = new_label; body = frame.body };;

  let create (body : Body.t) : t =
    let start_uid = Body.get_first_uid body in
    { curr_label = Uid(start_uid); body = body }
  ;;

end
;;

(* Exactly what it says on the tin. Corresponds to "T" in the grammar. *)
module Program_stack =
struct
  type t = Stack_frame.t list
  [@@deriving eq, ord, show]
  ;;

  let pop (s : t) : Stack_frame.t * t = List.hd s, List.tl s;;

  let push (s : t) (frame : Stack_frame.t) : t = frame :: s;;

  let top (s : t) : Stack_frame.t = List.hd s;;

  let empty = [];;
  let is_empty (s : t) = (s = []);;

  let singleton (frame : Stack_frame.t) = [frame];;
end
;;
(* A variable binding (x,m) indicates that the variable x is bound to
   memory location m. These correspond to "B" in the grammar *)
module Bindings : sig
  type t

  (* Add a binding of the input memloc to the given value, overwriting
     the previous entry if one exists *)
  val update_binding: t -> identifier -> memloc -> t

  (* Get the memory location bound to the current variable, if any *)
  val get_memloc: t -> identifier -> memloc option

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

  let get_memloc map id =
    try
      let m = Var_map.find id map in
      Some(m)
    with
    | Not_found -> None
  ;;

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
  val get_value: t -> memloc -> value option

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

  let get_value heap mem =
  try
    let v = Memloc_map.find mem heap.map in
    Some(v)
  with
  | Not_found -> None;;

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

  val get_binding: t -> eta -> Bindings.t

  val get_unbound_eta: t -> eta

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

  type t = { map: Bindings.t Eta_map.t; maxval: int };;

  let update_binding prev eta b =
    let newmap = Eta_map.add eta b prev.map in
    let Eta(etaval) = eta in
    let new_maxval = max etaval prev.maxval in
    { map = newmap; maxval = new_maxval };;

  let get_binding env eta =
    try
      let b = Eta_map.find eta env.map in
      b
    with
    | _ -> failwith "No binding found in environment";;

  let get_unbound_eta env = Eta(env.maxval + 1);;

  let empty = { map = Eta_map.empty; maxval = 0 };;

  let singleton eta b =
    let map = Eta_map.singleton eta b in
    let Eta(etaval) = eta in
    { map = map; maxval = etaval };;
end;;

(* The parents map holds the relationship between etas; children search their
   parent's corresponding binding if they encounter an unbound variable. *)
module Parents : sig
  type t

  (* Register the first input eta as a child of the second *)
  val add_parent: t -> eta -> eta -> t

  (* Retrieve the parent of the input eta, if any *)
  val get_parent: t -> eta -> eta option

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

  let get_parent map child =
    try
      let parent = Eta_map.find child map in
      Some(parent)
    with
    | Not_found -> None;;

  let empty = Eta_map.empty;;

  let singleton child parent = Eta_map.singleton child parent;;
end;;

type program =
  {
    stack: Program_stack.t;
    heap: Heap.t;
    env: Environment.t;
    parents: Parents.t;
    eta: eta
  }
