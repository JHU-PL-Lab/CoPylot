open Batteries;;
open Lamia_ast;;

type stmt = uid statement;;

module Stmt_ord =
struct
  type t = stmt;;

  let equal s1 s2 =
    let Statement(u1, _) = s1 in
    let Statement(u2, _) = s2 in
    equal_uid u1 u2
  ;;

  let compare s1 s2 =
    let Statement(u1, _) = s1 in
    let Statement(u2, _) = s2 in
    compare_uid u1 u2
  ;;
end

module Stmt_map = Map.Make(Stmt_ord);;
type relation_map = stmt option Stmt_map.t;;

let construct_left_triangle_map (prog : uid block) =
  let rec add_block map block =
    let Block(stmts) = block in
    let map, _ = List.fold_left construct_map (map, None) stmts in
    map

  and construct_map prev (stmt : stmt)=
    let (map, prev_stmt) = prev in
    match stmt with
    | Statement(_, d) ->
      let new_map = Stmt_map.add stmt prev_stmt map in
      let recursed_map =
        match d with
        | Lamia_ast.Let_expression (_, Function_expression(_, body))
        | Lamia_ast.While (_, body) ->
          add_block new_map body

        | Lamia_ast.Try_except (body, _, orelse)
        | Lamia_ast.Let_conditional_value (_, _, body, orelse)
        | Lamia_ast.Let_conditional_memory (_, _, body, orelse) ->
          let body_map = add_block new_map body in
          add_block body_map orelse

        | _ -> new_map
      in
      recursed_map, Some(stmt)
  in
  add_block Stmt_map.empty prog
;;

let construct_down_triangle_map (prog : uid block) =
  let rec add_block prev_stmt map block =
    let Block(stmts) = block in
    let map, _ = List.fold_left construct_map (map, prev_stmt) stmts in
    map

  and construct_map prev (stmt : stmt)=
    let (map, prev_stmt) = prev in
    match stmt with
    | Statement(_, d) ->
      let new_map = Stmt_map.add stmt prev_stmt map in
      let add_block = add_block (Some stmt) in
      let recursed_map =
        match d with
        | Lamia_ast.Let_expression (_, Function_expression(_, body))
        | Lamia_ast.While (_, body) ->
          add_block new_map body

        | Lamia_ast.Try_except (body, _, orelse)
        | Lamia_ast.Let_conditional_value (_, _, body, orelse)
        | Lamia_ast.Let_conditional_memory (_, _, body, orelse) ->
          let body_map = add_block new_map body in
          add_block body_map orelse

        | _ -> new_map
      in
      recursed_map, prev_stmt
  in
  add_block None Stmt_map.empty prog
;;

let construct_double_left_triangle_map (prog : uid block) =
  let rec add_block map block =
    let Block(stmts) = block in
    let map, _ = List.fold_left construct_map (map, None) stmts in
    map

  and construct_map prev (stmt : stmt)=
    let (map, prev_stmt_opt) = prev in
    let prev_stmt =
      if prev_stmt_opt = None then Some(stmt) else prev_stmt_opt
    in
    match stmt with
    | Statement(_, d) ->
      let new_map = Stmt_map.add stmt prev_stmt map in
      let recursed_map =
        match d with
        | Lamia_ast.Let_expression (_, Function_expression(_, body))
        | Lamia_ast.While (_, body) ->
          add_block new_map body

        | Lamia_ast.Try_except (body, _, orelse)
        | Lamia_ast.Let_conditional_value (_, _, body, orelse)
        | Lamia_ast.Let_conditional_memory (_, _, body, orelse) ->
          let body_map = add_block new_map body in
          add_block body_map orelse

        | _ -> new_map
      in
      recursed_map, prev_stmt
  in
  add_block Stmt_map.empty prog
;;

(* Returns a tuple of the left, down and double-left maps, in that order *)
let construct_all_relation_maps (prog : uid block) =
  construct_left_triangle_map prog,
  construct_down_triangle_map prog,
  construct_double_left_triangle_map
;;
