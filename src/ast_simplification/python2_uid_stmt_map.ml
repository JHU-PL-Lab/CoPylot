open Python2_ast_types;;
module Abstract = Python2_abstract_ast;;
module Normalized = Python2_normalized_ast;;
open Uid_generation;;

let rec get_uid_hashtbl (m : Abstract.modl) =
  let open Abstract in
  match m with
  | Module(body, _) ->
    let tbl = collect_uids_stmt_lst (Uid_hashtbl.create 10) body in
    tbl

and collect_uids_stmt (tbl : Abstract.annotated_stmt Uid_hashtbl.t) {uid=u;exception_target=except;multi=in_loop;body} =
  let open Abstract in
  match body with
  | Assign (_, {body = Literal(FunctionVal(_, func_body)); _}) ->
    let new_tbl = collect_uids_stmt_lst tbl func_body in
    Uid_hashtbl.add new_tbl u {uid=u;exception_target=except;multi=in_loop;body=body}; new_tbl
  | Assign (_, _)
  | Return (_)
  | Print (_, _, _)
  | Raise (_)
  | Catch (_)
  | Pass
  | Goto (_)
  | GotoIfNot (_)
    ->
    Uid_hashtbl.add tbl u {uid=u;exception_target=except;multi=in_loop;body}; tbl

and collect_uids_stmt_lst tbl lst =
  List.fold_left collect_uids_stmt tbl lst
;;

let rec norm_get_uid_hashtbl (m : Normalized.modl) =
  let open Normalized in
  match m with
  | Module(body, _) ->
    let tbl = norm_collect_uids_stmt_lst (Uid_hashtbl.create 10) body in
    tbl

and norm_collect_uids_stmt (tbl : Normalized.annotated_stmt Uid_hashtbl.t) {uid=u;exception_target=except;multi=in_loop;body} =
  let open Normalized in
  match body with
  | Assign (_, {body = Literal(FunctionVal(_, func_body)); _}) ->
    let new_tbl = norm_collect_uids_stmt_lst tbl func_body in
    Uid_hashtbl.add new_tbl u {uid=u;exception_target=except;multi=in_loop;body=body}; new_tbl
  | Assign (_, _)
  | Return (_)
  | Print (_, _, _)
  | Raise (_)
  | Catch (_)
  | Pass
  | Goto (_)
  | GotoIfNot (_)
    ->
    Uid_hashtbl.add tbl u {uid=u;exception_target=except;multi=in_loop;body}; tbl

and norm_collect_uids_stmt_lst tbl lst =
  List.fold_left norm_collect_uids_stmt tbl lst
;;
