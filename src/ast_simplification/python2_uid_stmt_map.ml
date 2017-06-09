open Python2_normalized_ast;;
open Uid_generation;;

let rec get_uid_hashtbl (m : modl) =
  match m with
  | Module(body, _) ->
    let tbl = collect_uids_stmt_lst (Uid_hashtbl.create 10) body in
    tbl

and collect_uids_stmt (tbl : stmt Uid_hashtbl.t) (s : stmt annotation) =
  match s with
  | Assign (_, _, u, _)
  | Return (_, u, _)
  | Print (_, _, _, u, _)
  | Raise (_, u, _)
  | Catch (_, u, _)
  | Pass (u, _)
  | Goto (_, u, _)
  | GotoIfNot (_, u, _, _)
  | SimpleExprStmt (_, u, _)
    ->
    Uid_hashtbl.add tbl u s; tbl
  | FunctionDef (_, _, body, u, _) ->
    let new_tbl = collect_uids_stmt_lst tbl body in
    Uid_hashtbl.add new_tbl u s; new_tbl

and collect_uids_stmt_lst tbl lst =
  List.fold_left collect_uids_stmt tbl lst
;;
