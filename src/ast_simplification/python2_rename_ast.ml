open Batteries
open Python2_ast

(* module ID_tuple = struct
  type t = identifier * expr_context
  let compare (id1,ctx1) (id2,ctx2) =
    match (String.compare id1 id2) with
    | 0 ->
      let compare_ctx c1 c2 = match (c1, c2) with
        | (Load,Load) | (Store,Store) | (Param,Param) | (AugStore,AugStore) -> 0
        | _ -> -1
      in compare_ctx ctx1 ctx2
    | n -> n
end
;; *)

(* Mapping utilities *)
(* Maps: id * ctx -> id *)
module Id_map = Map.Make (String);;

(* If a map does not exist, do nothing *)
let safe_map id id_map =
  try Id_map.find id id_map with
    Not_found -> id
;;

(* Let us add maps with regard to context *)
let safe_add id new_id id_map =
  (* match ctx with
     | Param ->
     let new_map = Id_map.add (id,ctx) path id_map in
     let new_map = Id_map.add (id,Store) path new_map in
     Id_map.add (id,Load) path new_map
     | Store ->
     let new_map = Id_map.add (id,ctx) path id_map in
     Id_map.add (id,Load) path new_map
     | _ -> id_map *)
  (* let new_map = Id_map.add (id,Param) new_id id_map in *)
  (* let new_map_2 = Id_map.add (id,Store) new_id new_map in *)
  Id_map.add id new_id id_map
;;

(* Getting (identifier, context) tuples of Nodes in the tree *)

(* The following functions should work in a certain scope such *)
(* that they are called within each Module/Function to get one *)
(* layer of identifiers. *)

let rec get_id_option = function
  | None -> []
  | Some x -> get_id_expr x

and get_id_stmt_list = function
  | [] -> []
  | [t] -> get_id_stmt t
  | hd :: tl -> (get_id_stmt hd) @ (get_id_stmt_list tl)

and get_id_expr_list = function
  | [] -> []
  | [t] -> get_id_expr t
  | hd :: tl -> (get_id_expr hd) @ (get_id_expr_list tl)

and get_id_stmt : 'a stmt -> (identifier * expr_context )list = function
  | FunctionDef (id,_,_,_,_) -> [(id,Store)]
  (* Uses Store for function Store type! *)
  | Return (target,_) -> get_id_option target
  | Assign (targets,value,_) ->
    (get_id_expr_list targets)
    @ (get_id_expr value)
  | AugAssign (target,_,value,_) ->
    (get_id_expr target)
    @ (get_id_expr value)
  | Print (dest,values,_,_) ->
    (get_id_option dest)
    @ (get_id_expr_list values)
  | For (target,iter,body,_,_) ->
    (get_id_expr target)
    @ (get_id_expr iter)
    @ (get_id_stmt_list body)
  (* Because flow controls don't have independent scopes, we intend *)
  (* to get all layers. *)
  | While (test,body,_,_) ->
    (get_id_expr test)
    @ (get_id_stmt_list body)
  | If (test,body,orelse,_) ->
    (get_id_expr test)
    @ (get_id_stmt_list body)
    @ (get_id_stmt_list orelse)
  | Expr (e,_) -> get_id_expr e (* Used in some lists; See big_test *)
  | _ -> []

and get_id_expr : 'a expr -> (identifier * expr_context) list = function
  | Name (id,ctx,_) -> [(id,ctx)]
  | BoolOp (_,values,_) -> get_id_expr_list values
  | BinOp (left,_,right,_) -> (get_id_expr left) @ (get_id_expr right)
  | UnaryOp (_,operand,_) -> get_id_expr operand
  | IfExp (test,body,orelse,_) ->
    (get_id_expr test)
    @ (get_id_expr body)
    @ (get_id_expr orelse)
  | Compare (left,_,comparators,_) ->
    (get_id_expr left)
    @ (get_id_expr_list comparators)
  | Call (func,args,_,_,_,_) ->
    (get_id_expr func)
    @ (get_id_expr_list args)
  | Subscript (value,slice,_,_) ->
    (get_id_expr value)
    @ (get_id_slice slice)
  | List (elts,_,_) -> (get_id_expr_list elts)
  | Tuple (elts,_,_) -> (get_id_expr_list elts)
  | _ -> [] (* Numbers, strings, etc. *)

(* The following gets id of params, specifically. *)
and get_id_arguments : 'a arguments -> (identifier * expr_context) list = function
  | (args,_,_,_) -> get_id_expr_list args

and get_id_slice : 'a slice -> (identifier * expr_context) list = function
  | Slice (lower,upper,step) ->
    (get_id_option lower)
    @ (get_id_option upper)
    @ (get_id_option step)
  | Index value -> get_id_expr value
;;

(* Reconstuct tree Nodes from an id map and existent Nodes *)
(* identifier -> 'a expr -> 'a expr *)
let rec make_option id_map = function
  | Some x -> Some (make_expr id_map x)
  | None -> None

(* Reconstuct a layer of the tree *)
and make_stmt_list id_map lst = List.map (make_stmt id_map) lst

and make_expr_list id_map lst= List.map (make_expr id_map) lst

and make_stmt id_map = function
  | FunctionDef (id,args,body,dec,a) ->
    let alias = safe_map id id_map in
    FunctionDef (alias,args,body,dec,a)
  | Return (value,a) -> Return (make_option id_map value,a)
  | Assign (targets,value,a) ->
    Assign (make_expr_list id_map targets,
            make_expr id_map value,a)
  | AugAssign (target,op,value,a) ->
    AugAssign (make_expr id_map target,op,
               make_expr id_map value,a)
  | Print (dest, values, nl, a) ->
    Print (make_option id_map dest,
           make_expr_list id_map values,nl,a)
  | For (target, iter, body, orelse, a) ->
    For (make_expr id_map target,
         make_expr id_map iter,
         make_stmt_list id_map body,orelse,a)
  | While (test, body, orelse, a) ->
    While (make_expr id_map test,
           make_stmt_list id_map body,orelse,a)
  | If (test, body, orelse, a) ->
    If (make_expr id_map test,
        make_stmt_list id_map body,orelse,a)
  | Expr (value, a) ->
    Expr (make_expr id_map value,a)
  | s -> s

and make_expr id_map = function
  | Name (id,ctx,a) ->
    let alias = safe_map id id_map in
    Name (alias,ctx,a)
  | BoolOp (op,values,a) -> BoolOp (op,make_expr_list id_map values,a)
  | BinOp (left,op,right,a) ->
    BinOp (make_expr id_map left,op,
           make_expr id_map right,a)
  | UnaryOp (op,operand,a) -> UnaryOp (op,make_expr id_map operand,a)
  | IfExp (test,body,orelse,a) ->
    IfExp (make_expr id_map test,
           make_expr id_map body,
           make_expr id_map orelse,a)
  | Compare (left,ops,comparators,a) ->
    Compare (make_expr id_map left,ops,make_expr_list id_map comparators,a)
  | Call (func,args,k,s,kw,a) ->
    Call (make_expr id_map func,make_expr_list id_map args,k,s,kw,a)
  | Subscript (value,slice,ctx,a) ->
    Subscript (make_expr id_map value,make_slice id_map slice,ctx,a)
  | List (elts,ctx,a) -> List (make_expr_list id_map elts,ctx,a)
  | Tuple (elts,ctx,a) -> Tuple (make_expr_list id_map elts,ctx,a)
  | e -> e

and make_slice id_map = function
  | Slice (lower,upper,step) ->
    Slice (make_option id_map lower,make_option id_map upper,make_option id_map step)
  | Index (value) -> Index (make_expr id_map value)

and make_arguments id_map (args,v,k,d) =
  ((make_expr_list id_map args),v,k,d)
;;

(* Mapping update *)
(* These functions are called at each level of scope: *)

(* Updates the address by appending "<new_scope_name>$<line_number>_" to the front *)
let update_address id address a =
  (id^"$"^(string_of_int (Pos.to_pos a).Lexing.pos_lnum)) :: address
;;

(* Given the id_list at each level of recursion, update id_map with id and path *)
(* Names of "Param" contexts are named with $param. *)
let rec update_map id_map id_list address =
  let path id _ =
    String.concat "_" [(List.hd address);id] in
  match id_list with
  | [] -> id_map
  | (id,ctx) :: rest -> update_map (safe_add id (path id ctx) id_map) rest address

(* Filter out all id with context "Load". *)
let rec filter_id = function
  | [] -> []
  | (id,ctx) :: rest ->
    if ctx == Load
    then filter_id rest
    else (id,ctx) :: (filter_id rest)
;;

(* Main recursion *)
(* id_map: (id * id) map.t ->     Maps to be applied to current frame.  *)
(* address: string list ->        Address stack of scopes.              *)
(* node: 'a stmt list ->          Node from original tree.              *)
(* return: 'a stmt list           New AST.                              *)

let rec rename_modl id_map address (node : 'a modl) =
  match node with
  | Module (body,a) -> Module ((rename_stmt_list id_map address body),a)

and rename_stmt_list id_map address (node : 'a stmt list) =
  match node with
  | [] -> []
  | stmt :: rest ->
    (* Idea: if the stmt is a function, call its own recursion. *)
    (* If the stmt has nested stmt lists, recurse on those. *)
    (* Otherwise, rename. *)
    let get_rename_method = function
      | FunctionDef (id,args,body,dec,a) ->
        rename_stmt_func id_map address (FunctionDef (id,args,body,dec,a))
      | For (target, iter, body, orelse, a) ->
        For (make_expr id_map target,
             make_expr id_map iter,
             rename_stmt_list id_map address body,orelse,a)
      | While (test, body, orelse, a) ->
        While (make_expr id_map test,
               rename_stmt_list id_map address body,orelse,a)
      | If (test, body, orelse, a) ->
        If (make_expr id_map test,
            rename_stmt_list id_map address body,orelse,a)
      | other ->
        rename_stmt_simple id_map other
    in (get_rename_method stmt) :: rename_stmt_list id_map address rest

and rename_stmt_func id_map address (node : 'a stmt) =
  match node with
  | FunctionDef (id,args,body,dec,a) ->
    (* Order of getting id is important. *)
    let id_list = filter_id ((get_id_arguments args) @ (get_id_stmt_list body)) in
    let new_address = update_address id address a in
    let new_map = update_map id_map id_list new_address in
    (* The function is renamed using the former map *)
    FunctionDef (safe_map id id_map,
                 make_arguments new_map args,
                 rename_stmt_list new_map new_address body,
                 dec,a)
  | _ -> failwith "Wrong way: FunctionDef."

and rename_stmt_simple id_map (node : 'a stmt) =
  make_stmt id_map node
;;
