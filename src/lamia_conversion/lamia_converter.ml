open Batteries;;
open Lamia_ast;;
open Python2_normalized_ast;;
open Python2_ast_types;;
open Unique_name_ctx;;
open Uid_ctx;;
open Lamia_conversion_monad;;
open Lamia_conversion_builtin_names;;
open Lamia_conversion_builtin_defs;;
open Lamia_conversion_preamble;;
open Lamia_conversion_utils;;
open Lamia_conversion_object_defs;;
open Lamia_conversion_add_uids;;

open Conversion_monad;;

let rec convert_module
    (ctx : name_context)
    (m : modl)
  : annot block =
  let Module(stmts) = m in
  let annot = Python2_ast.Pos.of_pos Lexing.dummy_pos in
  let preamble_ctx = create_new_name_ctx 0 "$preamble_" in
  let _, lamia_preamble = run preamble_ctx annot preamble in
  let _, lamia_prog = run ctx annot @@ convert_stmt_list stmts in
  let annot_block = Block(lamia_preamble @ lamia_prog) in
  annot_block

and convert_stmt_list (stmts : annotated_stmt list) : unit m =
  let accumulate m s = bind m (fun () -> convert_stmt s) in
  List.fold_left accumulate empty stmts

and convert_stmt
    (s : annotated_stmt)
  : unit m =
  local_annot s.annot @@
  match s.body with
  | Assign (id, value) ->
    let%bind value_result = convert_expr value in
    assign_python_variable id value_result

  | Return (x) ->
    let%bind lookup_result = lookup x in
    emit
      [
        Lamia_ast.Return(lookup_result);
      ]

  | While (test, body) ->
    let%bind value_result, value_stmts =
      listen @@ lookup_and_get_attr "*value" test
    in
    (* Work to figure out if test is True or False. Needs to be
       appended to the while loop body as well *)
    let value_bindings =
      (* Turn value_bindings back into directives *)
      List.map (fun s -> let Statement(_, d) = s in d) value_stmts
    in
    let%bind _, while_body =
      listen @@
      let%bind _ = convert_stmt_list body in
      emit value_bindings
    in
    emit @@
    value_bindings @
    [
      Lamia_ast.While(value_result,
                      Block(while_body));
    ]

  | If (test, body, orelse) ->
    let%bind value_result = lookup_and_get_attr "*value" test in

    let dummy_return =
      emit
        [
          If_result_value(builtin_none);
        ]
    in

    let%bind _, new_body =
      listen @@
      let%bind _ = convert_stmt_list body in
      dummy_return
    in
    let%bind _, new_orelse =
      listen @@
      let%bind _ = convert_stmt_list orelse in
      dummy_return
    in

    let%bind test_result = fresh_value_var () in
    let%bind dummy_variable = fresh_value_var () in

    emit @@
    [
      Let_get(test_result, value_result);
      Let_conditional_value(dummy_variable,
                            test_result,
                            Block(new_body),
                            Block(new_orelse));
    ]

  | Raise (x) ->
    let%bind lookup_result = lookup x in
    emit
      [
        Lamia_ast.Raise(lookup_result);
      ]

  | TryExcept (body, exn_name, handler) ->
    let%bind exn_memloc = fresh_memory_var () in
    let%bind _, new_body = listen @@ convert_stmt_list body in
    let%bind _, new_handler =
      listen @@
      let%bind _ = assign_python_variable exn_name exn_memloc in
      convert_stmt_list handler
    in
    emit
      [
        Try_except(Block(new_body),
                   exn_memloc,
                   Block(new_handler))
      ]

  | Pass ->
    empty

  | Break ->
    raise @@ Jhupllib_utils.Not_yet_implemented "Convert break stmt"

  | Continue ->
    raise @@ Jhupllib_utils.Not_yet_implemented "Convert continue stmt"

and convert_expr
    (e : annotated_expr)
  : memory_variable m =
  local_annot e.annot @@
  match e.body with
  | Binop (left, op, right) ->
    let%bind left_result = lookup left in
    let%bind right_result = lookup right in
    let%bind value_result =
      match op with
      | Is ->
        let%bind result = fresh_value_var () in
        let%bind _ = emit
            [
              Let_is(result, left_result, right_result);
            ]
        in
        return result
    in
    let%bind obj_result = wrap_bool value_result in
    return obj_result

  | UnaryOp (op, value) ->
    let%bind value_result = lookup_and_get_attr "*value" value in
    let%bind op_result =
      match op with
      | Not ->
        let%bind boolval = fresh_value_var () in
        let%bind result = fresh_value_var () in
        let%bind _ =  emit
            [
              Let_get(boolval, value_result);
              Let_unop(result, Unop_not, boolval);
            ]
        in
        return result
    in
    let%bind obj_result = wrap_bool op_result in
    return obj_result

  | Call (func, args) ->
    let%bind lookup_result = lookup func in
    let%bind arg_results = convert_list lookup args in
    let%bind callable = get_call lookup_result in
    let%bind arg_list = store_value @@ List_expression arg_results in
    let%bind retval = fresh_memory_var () in
    let%bind _ = emit
        [
          Let_call_function(retval, callable, [arg_list]);
        ]
    in
    return retval

  | Attribute (obj, attr) ->
    (* TODO: When we add inheritance, lamia get_attr will no longer be
       the same the python . operator. At that point lookup_and_get_attr
       won't work; we'll need to lookup, then do some complicated stuff
       to use the __getattr__ function and follow the inheritance chain *)
    let%bind obj_loc = lookup obj in
    let%bind attr_result =
      let%bind obj_val = fresh_value_var () in
      let%bind _ = emit
          [
            Let_get(obj_val, obj_loc)
          ]
      in
      get_attr attr obj_val
    in
    (* Because we don't have classes yet, we can't store an actual method object
       during object creation (since we'd have to put methods on that, and on
       its methods, etc). So we have to wrap methods on projection.
       This is really hacky and I hate it, but I see no alternative. *)
    let%bind attr_val = get_value attr_result in
    let%bind is_method = fresh_value_var () in
    let%bind _ =
      emit
        [
          Let_unop(is_method, Unop_is_function, attr_val);
        ]
    in
    let%bind final_result = fresh_memory_var () in
    let%bind _ =
      let%bind _, on_success =
        listen @@
        let%bind wrapped_method = wrap_method attr_val obj_loc in
        emit
          [
            If_result_memory wrapped_method;
          ]
      in
      let%bind _, on_failure =
        listen @@
        emit
          [
            If_result_memory attr_result;
          ]
      in
      emit
        [
          Let_conditional_memory(final_result,
                                 is_method,
                                 Block on_success,
                                 Block on_failure)
        ]
    in
    return final_result

  | List (elts) ->
    let%bind elt_results = convert_list lookup elts in
    let%bind list_val = store_value @@ List_expression elt_results in
    let%bind obj_result = wrap_list list_val in
    return obj_result

  | Tuple (elts) ->
    let%bind elt_results = convert_list lookup elts in
    let%bind tuple_val = store_value @@ List_expression elt_results in
    let%bind obj_result = wrap_tuple tuple_val in
    return obj_result

  | Num (num) ->
    begin
      match num with
      | Python2_ast_types.Int n ->
        let%bind value = store_value @@ Integer_literal n in
        let%bind obj_result = wrap_int value in
        return obj_result

      | Python2_ast_types.Float _ ->
        raise @@ Jhupllib_utils.Not_yet_implemented "wrap_float"
    end

  | Str (s) ->
    let%bind value = store_value @@ String_literal s in
    let%bind obj_result = wrap_string value in
    return obj_result

  | Bool (b) ->
    let%bind value = store_value @@ Boolean_literal b in
    let%bind obj_result = wrap_bool value in
    return obj_result

  | Builtin (b) ->
    begin
      match b with
      | Builtin_slice          -> return builtin_slice
      | Builtin_bool           -> return builtin_bool
      | Builtin_AttributeError -> return builtin_AttributeError
      | Builtin_ValueError     -> return builtin_ValueError
      | Builtin_TypeError      -> return builtin_TypeError
      | Builtin_StopIteration  -> return builtin_StopIteration
    end

  | FunctionVal (args, body) ->
    let%bind lamia_argname = fresh_value_var () in
    let%bind _, converted_body = listen @@
      let gen_arg_binding m argname =
        let%bind list_val, scopename, index = m in
        let%bind new_scopename = fresh_value_var () in
        let%bind argname_value = fresh_value_var () in
        let%bind list_result = extract_nth_list_elt list_val index in
        let%bind _ =
          emit
            [
              Let_expression(argname_value, String_literal argname);
              Let_binding_update(new_scopename, scopename, argname_value, list_result);
            ]
        in
        return (list_val, new_scopename, index + 1)
      in

      let%bind empty_scope = fresh_value_var () in
      let%bind get_from_scope_val = get_from_scope_def in
      let%bind _ =
        emit
          [
            Let_alias_value(get_from_parent_scope, get_from_scope);
            Let_alloc(python_scope);
            Let_expression(get_from_scope, get_from_scope_val);
            Let_expression(empty_scope, Empty_binding);
          ]
      in

      let%bind _, final_scope, _ =
        List.fold_left gen_arg_binding
          (return (lamia_argname, empty_scope, 0))
          args
      in

      let%bind _ = emit
          [
            Store(python_scope, final_scope);
          ]
      in

      convert_stmt_list body
    in

    let lamia_funcval =
      Function_expression([lamia_argname], Block converted_body)
    in

    let%bind funcval_name = fresh_value_var () in
    let%bind funcval_loc = fresh_memory_var () in
    let%bind _ = emit
        [
          Let_expression(funcval_name, lamia_funcval);
          Let_alloc(funcval_loc);
          Store(funcval_loc, funcval_name);
        ]
    in
    return funcval_loc

  | Name (id) ->
    lookup id
;;

let annot_to_uid
    (b : annot block)
  : uid block * uid_context =
  let uid_ctx = create_new_uid_ctx 0 in
  let uid_block = add_uids_block uid_ctx b in
  uid_block, uid_ctx
;;
