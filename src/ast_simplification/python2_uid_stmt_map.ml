open Python2_ast_types;;
open Python2_abstract_ast;;
open Uid_generation;;

let rec get_uid_hashtbl (m : modl) =
  match m with
  | Module(body, _) ->
    let tbl = collect_uids_stmt_lst (Uid_hashtbl.create 10) body in
    tbl

and collect_uids_stmt (tbl : annotated_stmt Uid_hashtbl.t) {uid=u;exception_target=except;multi=in_loop;body} =
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
