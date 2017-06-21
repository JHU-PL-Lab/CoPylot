open Batteries;;
open Python2_normalized_ast;;

type uid = Python2_ast_types.uid
[@@deriving eq, ord, show]
;;

type identifier = Python2_ast_types.identifier
[@@deriving eq, ord, show]
;;

type number = Python2_ast_types.number
[@@deriving eq, ord, show]
;;

type str = Python2_ast_types.str
[@@deriving eq, ord, show]
;;

type label =
  | Uid of uid
  | End
[@@deriving eq, ord, show]
;;

type builtin_exception =
  | Builtin_AttributeError
  | Builtin_NameError
  | Builtin_ValueError
[@@deriving eq, ord, show]
;;

type builtin_function =
  | Builtin_bool
  | Builtin_slice
  | Builtin_type
[@@deriving eq, ord, show]
;;

type builtin_method =
  | Builtin_call
[@@deriving eq, ord, show]
;;

(* A location in memory. Corresponds to "m" in the grammar. *)
type memloc =
  | Memloc of int
  | Builtin_exn_memloc of builtin_exception
  | Builtin_fun_memloc of builtin_function
  | Builtin_method_memloc of builtin_method
[@@deriving eq, ord, show]
;;

(* If the memloc is not a builtin, get its value *)
let memloc_to_int = function
  | Memloc(v) -> v
  | _ -> 0
;;

(* A variable binding (x,m) indicates that the variable x is bound to
   memory location m. These correspond to "B" in the grammar *)
module Bindings : sig
  type t

  (* Add a binding of the input memloc to the given value, overwriting
     the previous entry if one exists *)
  val update_binding: identifier -> memloc -> t -> t

  (* Get the memory location bound to the current variable, if any *)
  val get_memloc: identifier -> t -> memloc option

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

  let update_binding id mem prev = Var_map.add id mem prev;;

  let get_memloc id map =
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
  val get_stmt: t -> uid -> annotated_stmt option

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

  open Python2_ast_types;;

  let get_first_uid b =
    let fst = List.hd b.stmts in fst.uid;;

  let get_stmt (b : t) (u : uid) =
    try
      Some(List.find (fun s -> (s.uid = u)) b.stmts)
    with
    | Not_found -> None

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

  (* Retrieve the method with the specified uid, if one exists *)
  val get_stmt: t -> uid -> annotated_stmt option
end =
struct
  type t = { curr_label: label; body: Body.t }
  [@@deriving eq, ord, show]
  ;;
  ignore @@ show;; (* The fact that we need this is definitely a bug *)

  let active_stmt (frame : t) =
    match frame.curr_label with
    | End -> None
    | Uid u ->
      let active = Body.get_stmt frame.body u in
      match active with
      | None -> failwith "Stack frame uid not contained in body"
      | Some _ -> active
  ;;

  let get_next_label (frame : t) =
    let next = Body.get_next_label frame.body frame.curr_label in
    match next with
    | None -> failwith "Can't move past end of stack frame"
    | Some(l) -> l
  ;;

  let advance (frame : t) (new_label : label) : t =
    match new_label with
    | End -> { curr_label = End; body = frame.body }
    | Uid(uid) ->
      match Body.get_stmt frame.body uid with
      | None -> failwith "Tried to goto a uid not in the stack frame"
      | Some _ ->
        { curr_label = new_label; body = frame.body }
  ;;

  let create (body : Body.t) : t =
    let start_uid = Body.get_first_uid body in
    { curr_label = Uid(start_uid); body = body }
  ;;

  let get_stmt (frame: t) (uid: uid) : annotated_stmt option =
    Body.get_stmt frame.body uid
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

type function_val =
  | Builtin_func of builtin_function
  | User_func of memloc (* Bound scope *) * identifier list (* arglist *) * Body.t
[@@deriving eq, ord, show]
;;

type method_val =
  | Builtin_method of memloc (* self *) * builtin_method
  | User_method of memloc (* self *) * memloc (* Bound scope *) * identifier list (* args *) * Body.t

(* The possible values in the program. Corresponds to "v" in the grammar. *)
type value =
  | Bindings of Bindings.t
  | Num of number
  | Str of str
  | Bool of bool
  | ListVal of memloc list
  | TupleVal of memloc list
  | Builtin_exception of builtin_exception
  | Function of function_val
  | Method of method_val
  | NoneVal
  (* [@@deriving eq, ord, show] *)
;;

(* A heap binding (m,v) indicates that the memory location m contains the
   value v. These correspond to "H" in the grammar *)
module Heap: sig
  type t

  (* Return a memloc which does not yet appear in the heap *)
  val get_new_memloc: t -> memloc

  (* Add a binding of the input memloc to the given value, overwriting
     the previous entry if one exists *)
  val update_binding: memloc -> value -> t -> t

  (* Get the value bound to the given location in memory *)
  val get_value: memloc -> t -> value

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

  let update_binding mem value heap =
    let new_map = Memloc_map.add mem value heap.map in
    let new_max = max heap.maxval (memloc_to_int mem) in
    { map = new_map; maxval = new_max };;

  let get_value mem heap =
    try
      let v = Memloc_map.find mem heap.map in
      v
    with
    | Not_found -> failwith "No value found in heap";;

  let empty = { map = Memloc_map.empty; maxval = 0 };;

  let singleton mem value =
    let map = Memloc_map.singleton mem value in
    let memval = memloc_to_int mem in
    { map = map; maxval = memval};;
end
;;

(* The parents map holds the relationship between memlocs; children search their
   parent's corresponding binding if they encounter an unbound variable. *)
module Parents : sig
  type t

  (* Register the first input memloc as a child of the second *)
  val add_parent: memloc -> memloc -> t -> t

  (* Retrieve the parent of the input memloc, if any *)
  val get_parent: memloc -> t -> memloc option

  val empty: t
  val singleton: memloc -> memloc -> t
end =
struct
  module Memloc_ord =
  struct
    type t = memloc
    let compare = compare_memloc
  end
  module Memloc_map = Map.Make(Memloc_ord);;

  type t = memloc Memloc_map.t;;

  let add_parent child parent prev = Memloc_map.add child parent prev;;

  let get_parent child map =
    try
      let parent = Memloc_map.find child map in
      Some(parent)
    with
    | Not_found -> None;;

  let empty = Memloc_map.empty;;

  let singleton child parent = Memloc_map.singleton child parent;;
end;;

type micro_inert =
  | Micro_var of identifier
  | Micro_memloc of memloc
  | Micro_value of value
  (* [@@deriving eq, ord, show] *)
;;

type micro_command =
  | STORE
  | WRAP
  | BIND
  | ADVANCE
  | LOOKUP
  | RAISE
  | POP
  | LIST of int (* size *)
  | TUPLE of int (* size *)
  | ALLOCNAMEERROR
  (* [@@deriving eq, ord, show] *)
;;

type micro_instruction =
  | Inert of micro_inert
  | Command of micro_command
  (* [@@deriving eq, ord, show] *)
;;

module Micro_instruction_stack :
sig
  type t

  val create: micro_instruction list -> t

  (* Returns the first non-inert micro_instruction in the list. Fails if
     there is none. If pop was called, also returns the stack with the that
     element removed *)
  val get_first_command: t -> micro_command
  val pop_first_command: t -> micro_command * t

  (* Returns the inert micro_instruction immediately before the first command
     in the stack. Fails if there is none.
     If pop was called, also returns the stack with the that element removed *)
  val get_first_inert: t -> micro_inert
  val pop_first_inert: t -> micro_inert * t

  val is_empty: t -> bool

  (* Insert the contents of the second stack immediately before the first
     non-inert in the second stack.

     The parts of the first stack before and after the site of the location
      are unaffected. *)
  val insert: t -> t -> t

end =
struct
  (* TODO: replace these lists with an actual stack data type? *)
  type t = micro_inert list * micro_instruction list

  (* We maintain the invariant that we are always pointing to the first
     non-inert micro_instruction in the stack; that is, that the first element
     of the micro_instruction list is not inert *)

  let get_first_command (stack : t) : micro_command =
    match snd stack with
    | [] -> failwith "No commands in MI stack"
    | hd::_ ->
      match hd with
      | Inert _ -> failwith "Inert was at the head of our MI stack!"
      | Command c -> c
  ;;

  let pop_first_command (stack : t) : micro_command * t =
    match snd stack with
    | [] -> failwith "No commands in MI stack"
    | hd::rest ->
      match hd with
      | Inert _ -> failwith "Inert was at the head of our MI stack!"
      | Command c -> c, (fst stack, rest)
  ;;

  let get_first_inert (stack : t) : micro_inert =
    match fst stack with
    | [] -> failwith "No inerts at beginning of MI stack"
    | hd::_ -> hd
  ;;

  let pop_first_inert (stack : t) : micro_inert * t =
    match fst stack with
    | [] -> failwith "Can't get from empty Micro instruction stack"
    | hd::rest ->
      hd, (rest, snd stack)
  ;;

  let is_empty (stack: t) =
    List.is_empty (fst stack) && List.is_empty (snd stack)
  ;;

  let create lst =
    let rec gather_inerts inerts lst =
      match lst with
      | [] -> [], []
      | hd::rest ->
        match hd with
        | Inert i -> gather_inerts (i::inerts) rest
        | _ -> inerts, rest
    in
    gather_inerts [] lst
  ;;

  let insert (stack: t) (inserted: t) =
    fst stack @ fst inserted, snd stack @ snd inserted
  ;;

end
;;

type program_state =
  {
    parents: Parents.t;
    micro: Micro_instruction_stack.t;
    stack: Program_stack.t;
    heap: Heap.t;
    eta: memloc
  }
