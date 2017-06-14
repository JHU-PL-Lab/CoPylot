open Python2_normalized_ast;;
open Uid_generation;;

let rec get_uid_hashtbl (m : modl) =
  match m with
  | Module(body, _) ->
    let tbl = collect_uids_stmt_lst (Uid_hashtbl.create 10) body in
    tbl

and collect_uids_stmt (tbl : annotated_stmt Uid_hashtbl.t) {uid=u;exception_target=except;multi=in_loop;body} =
  match body with
  | Assign (_, _)
  | Return (_)
  | Print (_, _, _)
  | Raise (_)
  | Catch (_)
  | Pass
  | Goto (_)
  | GotoIfNot (_)
  | NameStmt (_)
    ->
    Uid_hashtbl.add tbl u {uid=u;exception_target=except;multi=in_loop;body}; tbl
  | FunctionDef (_, _, func_body) ->
    let new_tbl = collect_uids_stmt_lst tbl func_body in
    Uid_hashtbl.add new_tbl u {uid=u;exception_target=except;multi=in_loop;body}; new_tbl

and collect_uids_stmt_lst tbl lst =
  List.fold_left collect_uids_stmt tbl lst
;;
