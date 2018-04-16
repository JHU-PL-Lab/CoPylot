open Batteries;;
open Analysis_types;;

module Stmt_ord =
struct
  type t = statement;;

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
type relation_map = (statement option) Stmt_map.t;;

let construct_left_triangle_map (prog : block) =
  let rec add_block map block =
    let Block(stmts) = block in
    let map, _ = List.fold_left construct_map (map, None) (List.rev stmts) in
    map

  and construct_map prev (stmt : statement)=
    let (map, prev_stmt) = prev in
    match stmt with
    | Statement(_, d) ->
      let new_map = Stmt_map.add stmt prev_stmt map in
      let recursed_map =
        match d with
        | Let_expression (_, Function_expression(_, body)) ->
          add_block new_map body
        | While (_, body, orelse)
        | Let_conditional_value (_, _, body, orelse)
        | Let_conditional_memory (_, _, body, orelse) ->
          let body_map = add_block new_map body in
          add_block body_map orelse
        | Try_except (body, _, handlers, orelse) ->
          let body_map = add_block new_map body in
          let body_map' = add_block body_map handlers in
          add_block body_map' orelse
        | _ -> new_map
      in
      recursed_map, Some(stmt)
  in
  add_block Stmt_map.empty prog
;;

let construct_down_triangle_map (prog : block) =
  let rec add_block prev_stmt map block =
    let Block(stmts) = block in
    let map, _ = List.fold_left construct_map (map, prev_stmt) stmts in
    map

  and construct_map prev (stmt : statement) =
    let (map, prev_stmt) = prev in
    match stmt with
    | Statement(_, d) ->
      let new_map = Stmt_map.add stmt prev_stmt map in
      let add_block = add_block (Some stmt) in
      let recursed_map =
        match d with
        | Let_expression (_, Function_expression(_, body)) ->
          add_block new_map body

        | While (_, body, orelse)
        | Let_conditional_value (_, _, body, orelse)
        | Let_conditional_memory (_, _, body, orelse) ->
          let body_map = add_block new_map body in
          add_block body_map orelse
        | Try_except (body, _, handlers, orelse) ->
          let body_map = add_block new_map body in
          let body_map' = add_block body_map handlers in
          add_block body_map' orelse
        | _ -> new_map
      in
      recursed_map, prev_stmt
  in
  add_block None Stmt_map.empty prog
;;

let construct_double_left_triangle_map (prog : block) =
  let rec add_block map block =
    let Block(stmts) = block in
    let map, _ = List.fold_left construct_map (map, None) stmts in
    map

  and construct_map prev (stmt : statement)=
    let (map, prev_stmt_opt) = prev in
    let prev_stmt =
      if prev_stmt_opt = None then Some(stmt) else prev_stmt_opt
    in
    match stmt with
    | Statement(_, d) ->
      let new_map = Stmt_map.add stmt prev_stmt map in
      let recursed_map =
        match d with
        | Let_expression (_, Function_expression(_, body)) ->
          add_block new_map body

        | While (_, body, orelse)
        | Let_conditional_value (_, _, body, orelse)
        | Let_conditional_memory (_, _, body, orelse) ->
          let body_map = add_block new_map body in
          add_block body_map orelse
        | Try_except (body, _, handlers, orelse) ->
          let body_map = add_block new_map body in
          let body_map' = add_block body_map handlers in
          add_block body_map' orelse

        | _ -> new_map
      in
      recursed_map, prev_stmt
  in
  add_block Stmt_map.empty prog
;;

(* Returns a tuple of the left, down and double-left maps, in that order *)
type relation_map_record =
  {
    left: relation_map;
    down: relation_map;
    double_left: relation_map;
  }
;;

let construct_all_relation_maps (prog : block) =
  {
    left = construct_left_triangle_map prog;
    down = construct_down_triangle_map prog;
    double_left = construct_double_left_triangle_map prog;
  }
;;
