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

type builtin_exception =
  | Builtin_AttributeError
  | Builtin_NameError
  | Builtin_ValueError
  | Builtin_TypeError
[@@deriving eq, ord, show]
;;

type builtin_function =
  | Builtin_bool
  | Builtin_slice
  | Builtin_type_func
  | Builtin_call
[@@deriving eq, ord, show]
;;

type builtin_type =
  | Builtin_method_wrapper_type
[@@deriving eq, ord, show]
;;

(* A location in memory. Corresponds to "m" in the grammar. *)
type memloc =
  | Memloc of int
  | None_memloc
  | Builtin_exn_memloc of builtin_exception
  | Builtin_fun_memloc of builtin_function
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

  (* Return the uid of the next statement in the list, or None if none.
     WARNING: this is not necessarily the next statement to be executed if
     the first statement is a Goto or GotoIfNot statement.*)
  val get_next_uid: t -> uid -> uid option
end =
struct
  module Uid_ord =
  struct
    type t = uid
    let compare = compare_uid
  end
  module Uid_map = Map.Make(Uid_ord);;
  type lexical_map = uid Uid_map.t;;
  open Uid_generation;;

  type t = { stmts: annotated_stmt list;
             order: lexical_map;
             stmt_map: annotated_stmt Uid_hashtbl.t
           };;

  let compare t1 t2 = List.compare compare_annotated_stmt t1.stmts t2.stmts;;
  let equal t1 t2 = List.eq equal_annotated_stmt t1.stmts t2.stmts;;
  let pp fmt t1 = List.iter (pp_annotated_stmt fmt) t1.stmts;; (* TODO: Not sure this works *)

  open Python2_ast_types;;

  let get_first_uid b =
    let fst = List.hd b.stmts in fst.uid;;

  let get_stmt (b : t) (u : uid) =
    try
      Some(Uid_hashtbl.find b.stmt_map u)
    with
    | Not_found -> None

  let create (stmts : annotated_stmt list) =
    let rec add_to_map (map : lexical_map) (stmts : annotated_stmt list) : lexical_map =
      match stmts with
      | [] -> map

      | {uid = u1; body = b; _}::rst ->
        let deep_map =
          match b with
          | Assign(_, {body = Literal(FunctionVal(_, body)); _}) ->
            (* Recurse if necessary *)
            add_to_map map body
          | _ ->
            map
        in
        let next_map =
          match rst with
          | [] -> deep_map

          | {uid = u2; _}::_ ->
            Uid_map.add u1 u2 deep_map
        in
        add_to_map next_map rst
    in
    let lexical_map = add_to_map Uid_map.empty stmts in
    let stmt_map = Python2_uid_stmt_map.norm_get_uid_hashtbl @@ Module(stmts, 0) in

    { stmts = stmts; order = lexical_map; stmt_map = stmt_map; }
  ;;

  let get_next_uid (b : t) (u : uid) =
    try
      let next = Uid_map.find u b.order in
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
  val get_active_uid: t -> uid
  val get_eta: t -> memloc

  (* Advance the frame to the specified label *)
  val update_active_uid: t -> uid -> t

  (* Create a new stack frame from a memloc and start uid body *)
  val create: memloc -> uid -> t

end =
struct
  type t = { eta: memloc; curr_uid: uid }
  [@@deriving eq, ord, show]
  ;;
  ignore @@ show;; (* The fact that we need this is definitely a bug *)

  let get_active_uid (frame : t) = frame.curr_uid;;

  let get_eta (frame : t) = frame.eta;;

  let update_active_uid (frame : t) (new_uid : uid) : t =
    { frame with curr_uid = new_uid }
  ;;

  let create (eta: memloc) (start : uid) : t =
    { eta = eta; curr_uid = start }
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
  | User_func of memloc (* Bound scope *) * identifier list (* arglist *) * uid
[@@deriving eq, ord, show]
;;

(* The possible values in the program. Corresponds to "v" in the grammar. *)
type value =
  | Bindings of Bindings.t
  | Num of number
  | Str of str
  | Bool of bool
  | ListVal of memloc list
  | TupleVal of memloc list
  | Builtin_exception of builtin_exception
  | Builtin_type of builtin_type
  | Function of function_val
  | Method of memloc * function_val
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
  | LOOKUP
  | GET
  | ASSIGN
  | LIST of int (* size *)
  | TUPLE of int (* size *)
  | ADVANCE
  | POP
  | PUSH of uid
  | RAISE
  | GOTO of uid
  | GOTOIFNOT of uid
  | CALL of int (* numargs *)
  | CONVERT of int (* numargs *)
  | RETRIEVE
  | ALLOCNAMEERROR
  | ALLOCTYPEERROR
  | ALLOCATTRIBUTEERROR
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
    micro: Micro_instruction_stack.t;
    stack: Program_stack.t;
    heap: Heap.t;
  }

type program_context =
  {
    program: Body.t
  }
