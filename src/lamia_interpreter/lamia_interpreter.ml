open Batteries;;
open Jhupllib;;

open Lamia_evaluation_ast;;
open Lamia_evaluation_grammar;;
open Lamia_heap;;

type unary_operator = Lamia_ast.unary_operator;;
type binary_operator = Lamia_ast.binary_operator;;

type evaluation_result =
  | Evaluated_successfully
  | Evaluated_to_exception of memory_variable
  | Evaluation_error of string
;;

let value_of (e : value_expression) : value Lamia_heap_monad.m =
  let open Lamia_heap_monad in
  match e with
  | Integer_literal n -> return @@ Integer_value n
  | String_literal s -> return @@ String_value s
  | Boolean_literal b -> return @@ Boolean_value b
  | Function_expression(xs,b) -> return @@ Function_value(xs, b)
  | Lamia_evaluation_ast.List_value ys ->
    let%bind ms = sequence @@ List.map get_memory_address ys in
    return @@ Lamia_evaluation_grammar.List_value ms
  | Empty_binding -> return @@ Object_value StringMap.empty
  | None_literal -> return @@ None_value
;;

let assert_object_and_string (objv : value) (keyv : value) (action : string)
  : (memory_address StringMap.t * string) Lamia_heap_monad.m =
  let open Lamia_heap_monad in
  match objv,keyv with
  | Object_value dict, String_value keystr ->
    return (dict, keystr)
  | Object_value _, _ ->
    force_error @@ Printf.sprintf
      "Attempted to %s dictionary using non-string key %s"
      action (show_value keyv)
  | _, _ ->
    force_error @@ Printf.sprintf
      "Attempted to %s non-dictionary %s"
      action (show_value objv)
;;

let assert_list
    (v : value) (message : string) : memory_address list Lamia_heap_monad.m =
  let open Lamia_heap_monad in
  match v with
  | List_value lst ->
    return lst
  | _ ->
    force_error @@ Printf.sprintf "%s non-list %s" message (show_value v)
;;

let assert_integer
    (v : value) (message : string) : int Lamia_heap_monad.m =
  let open Lamia_heap_monad in
  match v with
  | Integer_value n ->
    return n
  | _ ->
    force_error @@ Printf.sprintf "%s non-integer %s" message (show_value v)
;;

let assert_integer_or_none
    (v : value) (fallback : int) (message : string) : int Lamia_heap_monad.m =
  let open Lamia_heap_monad in
  match v with
  | Integer_value n ->
    return n
  | None_value ->
    return fallback
  | _ ->
    force_error @@ Printf.sprintf "%s non-integer/non-None %s"
      message (show_value v)
;;

let assert_function (v : value) (message : string)
  : (value_variable list * block) Lamia_heap_monad.m =
  let open Lamia_heap_monad in
  match v with
  | Function_value(xs,b) ->
    return (xs,b)
  | _ ->
    force_error @@ Printf.sprintf "%s non-function %s" message (show_value v)
;;

let assert_in_bounds (n : int) (low : int) (high : int) (message : string)
  : unit Lamia_heap_monad.m =
  let open Lamia_heap_monad in
  if n < low || n > high then
    force_error @@
    Printf.sprintf "%s out-of-bounds index %d (expected in [%d,%d])"
      message n low high
  else
    return ()
;;

let assert_list_and_integer
    (listv : value) (indexv : value) (action : string) (include_end : bool)
  : (memory_address list * int) Lamia_heap_monad.m =
  let open Lamia_heap_monad in
  match listv,indexv with
  | List_value lst, Integer_value n ->
    if n < 0 || n > List.length lst ||
       (n == List.length lst && not include_end)
    then
      force_error @@ Printf.sprintf
        "Attempted to %s list using invalid index %s"
        action (show_value indexv)
    else
      return (lst, n)
  | Object_value _, _ ->
    force_error @@ Printf.sprintf
      "Attempted to %s list using non-integer index %s"
      action (show_value indexv)
  | _, _ ->
    force_error @@ Printf.sprintf
      "Attempted to %s non-list %s"
      action (show_value listv)
;;

let rec eval_step_directive (d : directive)
  : statement list Lamia_heap_monad.m =
  let open Lamia_heap_monad in
  match d with
  | Let_expression(x,e) ->
    let%bind v = value_of e in
    let%bind () = set_value x v in
    return []
  | Let_alloc y ->
    let%bind m = fresh_address () in
    let%bind () = set_memory_address y m in
    return []
  | Let_alias_value(x1,x2) ->
    let%bind v = get_value x1 in
    let%bind () = set_value x2 v in
    return []
  | Let_alias_memory(y1,y2) ->
    let%bind m = get_memory_address y1 in
    let%bind () = set_memory_address y2 m in
    return []
  | Let_binding_update(x,obj,key,y) ->
    let%bind objv = get_value obj in
    let%bind keyv = get_value key in
    let%bind dict,keystr = assert_object_and_string objv keyv "update" in
    let%bind m = get_memory_address y in
    let dict' = StringMap.add keystr m dict in
    let%bind () = set_value x (Object_value dict') in
    return []
  | Let_binding_access(y,obj,key) ->
    let%bind objv = get_value obj in
    let%bind keyv = get_value key in
    let%bind dict,keystr = assert_object_and_string objv keyv "access" in
    begin
      match StringMap.Exceptionless.find keystr dict with
      | None ->
        force_error @@ Printf.sprintf
          "Failed to look up key %s in dictionary %s"
          (show_value keyv)
          (show_value objv)
      | Some m ->
        let%bind () = set_memory_address y m in
        return []
    end
  | Let_list_access(y,x1,x2) ->
    let%bind listv = get_value x1 in
    let%bind idxv = get_value x2 in
    let%bind ms = assert_list listv "Attempted list access with" in
    let%bind idx = assert_integer idxv "Attempted list access with" in
    let%bind () =
      assert_in_bounds idx 0 (List.length ms - 1) "Attempted list access with"
    in
    let m = List.nth ms idx in
    let%bind () = set_memory_address y m in
    return []
  | Let_list_slice(x0,xlst,xstart,xend) ->
    let%bind listv = get_value xlst in
    let%bind idxv1 = get_value xstart in
    let%bind idxv2 = get_value xend in
    let%bind ms = assert_list listv "Attempted list slice with" in
    let%bind idx1 =
      assert_integer_or_none idxv1 0 "Attempted list slice with"
    in
    let%bind idx2 =
      assert_integer_or_none idxv2 (List.length ms) "Attempted list slice with"
    in
    let ms' = List.take (idx2 - idx1) @@ List.drop idx1 ms in
    let%bind () = set_value x0 @@ List_value ms' in
    return []
  | Let_call_function(y,x0,args) ->
    let%bind v = get_value x0 in
    let%bind (params,Block body) = assert_function v "Attempted to call" in
    if List.length params != List.length args then
      force_error @@
      Printf.sprintf
        "Wrong number of arguments at all: found %d, expected %d for function %s"
        (List.length args) (List.length params) (show_value v)
    else
      let arg_wiring =
        List.combine params args
        |> List.map (fun (p,a) -> Statement(Let_alias_value(p,a)))
      in
      let function_block = Block(arg_wiring @ body) in
      let freshened_block = Lamia_freshener.freshen_block_top function_block in
      return [Active_fun_block(y, freshened_block)]
  | Store(y, x) ->
    let%bind m = get_memory_address y in
    let%bind v = get_value x in
    let%bind () = set_heap_value m v in
    return []
  | Let_get(x, y) ->
    let%bind m = get_memory_address y in
    let%bind v = get_heap_value m in
    let%bind () = set_value x v in
    return []
  | Let_is(x,y1,y2) ->
    let%bind m1 = get_memory_address y1 in
    let%bind m2 = get_memory_address y2 in
    let%bind () = set_value x @@ Boolean_value(equal_memory_address m1 m2) in
    return []
  | Let_unop(x,unop,x') ->
    let%bind v' = get_value x' in
    let%bind v =
      begin
        match unop, v' with
        | Lamia_ast.Unop_not, Boolean_value b -> return @@ Boolean_value(not b)
        | Lamia_ast.Unop_not, _ ->
          force_error @@ Printf.sprintf "Invalid argument for \"not\": %s"
            (show_value v')
        | Lamia_ast.Unop_is_function, Function_value _ ->
          return @@ Boolean_value true
        | Lamia_ast.Unop_is_function, _ ->
          return @@ Boolean_value false
      end
    in
    let%bind () = set_value x v in
    return []
  | Let_binop(x0,x1,binop,x2) ->
    let%bind v1 = get_value x1 in
    let%bind v2 = get_value x2 in
    let%bind v0 =
      begin
        match binop with
        | Lamia_ast.Binop_intplus ->
          let%bind n1 =
            assert_integer v1 "Invalid first argument to integer addition:"
          in
          let%bind n2 =
            assert_integer v2 "Invalid second argument to integer addition:"
          in
          return @@ Integer_value(n1+n2)
        | Lamia_ast.Binop_intminus ->
          let%bind n1 =
            assert_integer v1 "Invalid first argument to integer subtraction:"
          in
          let%bind n2 =
            assert_integer v2 "Invalid second argument to integer subtraction:"
          in
          return @@ Integer_value(n1+n2)
        | Lamia_ast.Binop_haskey ->
          let%bind dict,keystr =
            assert_object_and_string v1 v2 "\"haskey\" for"
          in
          return @@ Boolean_value(StringMap.mem keystr dict)
        | Lamia_ast.Binop_listconcat ->
          let%bind m1 =
            assert_list v1 "Invalid first argument to list concatenation:"
          in
          let%bind m2 =
            assert_list v2 "Invalid second argument to list concatenation:"
          in
          return @@ Lamia_evaluation_grammar.List_value(m1 @ m2)
      end
    in
    let%bind () = set_value x0 v0 in
    return []
  | Return _ ->
    force_error "Encountered return without enclosing function"
  | If_result_value _ ->
    force_error "Encountered ifresult-value without enclosing conditional"
  | If_result_memory _ ->
    force_error "Encountered ifresult-memory without enclosing conditional"
  | Raise _ ->
    raise @@ Utils.Invariant_failure
      "Raise directive should've been captured by surrounding block processing"
  | Try_except(Block ss1,y,Block ss2) ->
    begin
      match ss1 with
      | [] -> return []
      | (Statement(Raise y'))::_ ->
        return @@ (Statement(Let_alias_memory(y,y')))::ss2
      | _ ->
        let%bind ss1' = eval_step_statement_list ss1 in
        return [ Statement(Try_except(Block ss1',y,Block ss2)) ]
    end
  | Let_conditional_value(xlet,xcond,bthen,belse) ->
    let%bind v = get_value xcond in
    begin
      match v with
      | Boolean_value true -> return @@ [Active_if_value_block(xlet,bthen)]
      | Boolean_value false -> return @@ [Active_if_value_block(xlet,belse)]
      | _ ->
        force_error @@
        Printf.sprintf "Non-boolean %s in conditional" (show_value v)
    end
  | Let_conditional_memory(ylet,xcond,bthen,belse) ->
    let%bind v = get_value xcond in
    begin
      match v with
      | Boolean_value true -> return @@ [Active_if_memory_block(ylet,bthen)]
      | Boolean_value false -> return @@ [Active_if_memory_block(ylet,belse)]
      | _ ->
        force_error @@
        Printf.sprintf "Non-boolean %s in conditional" (show_value v)
    end
  | While(y,Block ss) ->
    let%bind m = get_memory_address y in
    let%bind v = get_heap_value m in
    begin
      match v with
      | Boolean_value true ->
        return @@ ss @ [Statement(While(y, Block ss))]
      | Boolean_value false ->
        return []
      | _ ->
        force_error @@
        Printf.sprintf "Non-boolean %s as while condition" (show_value v)
    end

and eval_step_statement (s : statement)
  : statement list Lamia_heap_monad.m =
  let open Lamia_heap_monad in
  match s with
  | Statement(d) ->
    eval_step_directive d
  | Active_while_block(Block ss) ->
    begin
      match ss with
      | Statement(Return y) :: _ ->
        return @@ [Statement(Return y)]
      | Statement(Raise y) :: _ ->
        return @@ [Statement(Raise y)]
      | _ ->
        let%bind ss' = eval_step_statement_list ss in
        begin
          match ss' with
          | [] -> return []
          | _ -> return [Active_while_block(Block ss')]
        end
    end
  | Active_if_value_block(x, Block ss) ->
    begin
      match ss with
      | Statement(If_result_value x') :: _ ->
        return @@ [Statement(Let_alias_value(x,x'))]
      | Statement(Return y') :: _ ->
        return @@ [Statement(Return y')]
      | Statement(Raise y') :: _ ->
        return @@ [Statement(Raise y')]
      | [] ->
        force_error @@ "Reached end of if-value with no ifresult-value"
      | _ ->
        let%bind ss' = eval_step_statement_list ss in
        return @@ [Active_if_value_block(x, Block ss')]
    end
  | Active_if_memory_block(y, Block ss) ->
    begin
      match ss with
      | Statement(If_result_memory y') :: _ ->
        return @@ [Statement(Let_alias_memory(y,y'))]
      | Statement(Return y') :: _ ->
        return @@ [Statement(Return y')]
      | Statement(Raise y') :: _ ->
        return @@ [Statement(Raise y')]
      | [] ->
        force_error @@ "Reached end of if-memory with no ifresult-memory"
      | _ ->
        let%bind ss' = eval_step_statement_list ss in
        return @@ [Active_if_memory_block(y, Block ss')]
    end
  | Active_fun_block(y, Block ss) ->
    begin
      match ss with
      | Statement(Return y') :: _ ->
        return @@ [Statement(Let_alias_memory(y,y'))]
      | Statement(Raise y') :: _ ->
        return @@ [Statement(Raise y')]
      | [] ->
        force_error @@ "Reached end of function with no return"
      | _ ->
        let%bind ss' = eval_step_statement_list ss in
        return [Active_fun_block(y, Block ss')]
    end

and eval_step_statement_list (ss : statement list)
  : statement list Lamia_heap_monad.m =
  let open Lamia_heap_monad in
  match ss with
  | [] -> return []
  | s::ss' ->
    let%bind new_ss = eval_step_statement s in
    return @@ new_ss @ ss'
;;

(** Evaluates the program. *)
let rec eval (block : block) : evaluation_result Lamia_heap_monad.m =
  let open Lamia_heap_monad in
  let rec loop (ss : statement list) : evaluation_result Lamia_heap_monad.m =
    match ss with
    | [] ->
      return Evaluated_successfully
    | (Statement(Raise y)) :: _ ->
      return @@ Evaluated_to_exception y
    | _ ->
      let%bind ss' = eval_step_statement_list ss in
      loop ss'
  in
  let Block(ss) = block in
  loop ss
;;

let evaluate (ast : 'a Lamia_ast.block) : evaluation_result * Heap.t =
  let ast' = Lamia_evaluation_ast_converter.convert_block ast in
  let result = Lamia_heap_monad.run (eval ast') in
  let open Lamia_heap_monad in
  match result with
  | Lamia_heap_monad.Heap_error(s,h) -> (Evaluation_error s, h.heap)
  | Lamia_heap_monad.Heap_success(r,h) -> (r, h.heap)
;;
