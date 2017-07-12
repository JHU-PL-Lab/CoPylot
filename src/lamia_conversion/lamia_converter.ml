open Batteries;;
open Lamia_ast;;
open Python2_normalized_ast;;
open Lamia_conversion_ctx;;
open Lamia_conversion_builtin_names;;
open Lamia_conversion_preamble;;
open Lamia_conversion_utils;;
open Lamia_conversion_object_defs;;

let rec convert_module
    (ctx : conversion_context)
    (m : 'a modl)
  : block =
  let Module(stmts, _) = m in
  (* TODO: Preamble, scope setup, etc *)
  Block(map_and_concat (convert_stmt ctx) stmts)

and convert_stmt
    (ctx : conversion_context)
    (s : 'a stmt)
  : statement list =
  (* let convert_stmt = convert_stmt ctx in *)
  let annotate_directive annot d = annotate_directive ctx annot d in
  match s with
  | Assign (id, value, annot) ->
    let value_bindings, value_result = convert_expr ctx value in
    value_bindings @
    assign_python_variable ctx annot id value_result

  | Return (x, annot) ->
    let lookup_bindings, lookup_result = lookup ctx annot x in
    lookup_bindings @
    [
      annotate_directive annot @@
      Lamia_ast.Return(lookup_result);
    ]

  | While (test, body, annot) ->
    let value_bindings, value_result =
      lookup_and_get_attr ctx annot "*value" test
    in
    value_bindings @
    [
      annotate_directive annot @@
      Lamia_ast.While(value_result,
                      Block(map_and_concat (convert_stmt ctx) body @
                            value_bindings));
    ]

  | If (test, body, orelse, annot) ->
    let value_bindings, value_result =
      lookup_and_get_attr ctx annot "*value" test
    in
    let test_result = Value_variable(gen_unique_name ctx annot) in
    let test_bindings =
      value_bindings @
      [
        annotate_directive annot @@
        Let_get(test_result, value_result);
      ]
    in

    let dummy_variable = Value_variable(gen_unique_name ctx annot) in

    let dummy_return =
      let dummy_retval = Value_variable(gen_unique_name ctx annot) in
      List.map (annotate_directive annot)
        [
          Let_expression(dummy_retval, None_literal);
          If_result_value(dummy_retval);
        ]
    in

    let new_body =
      map_and_concat (convert_stmt ctx) body @ dummy_return
    in
    let new_orelse =
      map_and_concat (convert_stmt ctx) orelse @ dummy_return
    in

    test_bindings @
    [
      annotate_directive annot @@
      Let_conditional_value(dummy_variable,
                            test_result,
                            Block(new_body),
                            Block(new_orelse))
    ]

  | Raise (x, annot) ->
    let lookup_bindings, lookup_result = lookup ctx annot x in
    lookup_bindings @
    [
      annotate_directive annot @@
      Lamia_ast.Raise(lookup_result);
    ]

  | TryExcept (body, exn_name, handler, annot) ->
    let exn_memloc = Memory_variable(gen_unique_name ctx annot) in
    let new_body = map_and_concat (convert_stmt ctx) body in
    let new_handler =
      assign_python_variable ctx annot exn_name exn_memloc @
      map_and_concat (convert_stmt ctx) handler
    in
    [
      annotate_directive annot @@
      Try_except(Block(new_body),
                 exn_memloc,
                 Block(new_handler))
    ]

  | Pass _ ->
    []

  | Break _ ->
    raise @@ Jhupllib_utils.Not_yet_implemented "Convert break stmt"

  | Continue _ ->
    raise @@ Jhupllib_utils.Not_yet_implemented "Convert continue stmt"

and convert_expr
    (ctx : conversion_context)
    (s : 'a expr)
  : statement list * memory_variable =
  let annotate_directive annot d = annotate_directive ctx annot d in
  match s with
  | Binop (left, op, right, annot) ->
    let left_lookups, left_result = lookup ctx annot left in
    let right_lookups, right_result = lookup ctx annot right in
    let value_bindings, value_result =
      match op with
      | Is ->
        let result = Value_variable(gen_unique_name ctx annot) in
        left_lookups @ right_lookups @
        [
          annotate_directive annot @@
          Let_is(result, left_result, right_result);
        ],
        result
    in
    let obj_bindings, obj_result = wrap_bool ctx annot value_result in
    value_bindings @ obj_bindings, obj_result

  | UnaryOp (op, value, annot) ->
    let value_lookups, value_result =
      lookup_and_get_attr ctx annot "*value" value
    in
    let op_bindings, op_result =
      match op with
      | Not ->
        let boolval = Value_variable(gen_unique_name ctx annot) in
        let result = Value_variable(gen_unique_name ctx annot) in
        value_lookups @
        [
          annotate_directive annot @@
          Let_get(boolval, value_result);
          annotate_directive annot @@
          Let_unop(result, Unop_not, boolval);
        ],
        result
    in
    let obj_bindings, obj_result = wrap_bool ctx annot op_result in
    op_bindings @ obj_bindings, obj_result

  | Call (func, args, annot) ->
    (* TODO: Call *get_call instead of doing a direct lookup *)
    let lookup_bindings, lookup_result =
      lookup_and_get_attr ctx annot "*value" func
    in
    let arg_bindings, arg_results = convert_list (lookup ctx annot) args in
    let store_args, arg_list =
      store_value ctx annot @@
      List_value arg_results
    in
    let funcval = Value_variable(gen_unique_name ctx annot) in
    let retval = Memory_variable(gen_unique_name ctx annot) in
    let call_directives =
      [
        Let_get(funcval, lookup_result);
        Let_call_function(retval, funcval, [arg_list]);
      ]
    in
    let all_bindings =
      lookup_bindings @ arg_bindings @ store_args @
      List.map (annotate_directive annot) call_directives
    in
    all_bindings, retval

  | Attribute (obj, attr, annot) ->
    (* TODO: When we add inheritance, lamia get_attr will no longer be
       the same the python . operator. At that point lookup_and_get_attr
       won't work; we'll need to lookup, then do some complicated stuff
       to use the __getattr__ function and follow the inheritance chain *)
    lookup_and_get_attr ctx annot attr obj

  | List (elts, annot) ->
    let elt_bindings, elt_results = convert_list (lookup ctx annot) elts in
    let store_list, list_val =
      store_value ctx annot @@
      List_value elt_results
    in
    let obj_bindings, obj_result = wrap_list ctx annot list_val in
    elt_bindings @ store_list @ obj_bindings,
    obj_result

  | Tuple (elts, annot) ->
    let elt_bindings, elt_results = convert_list (lookup ctx annot) elts in
    let store_tuple, tuple_val =
      store_value ctx annot @@
      Tuple_value elt_results
    in
    let obj_bindings, obj_result = wrap_tuple ctx annot tuple_val in
    elt_bindings @ store_tuple @ obj_bindings,
    obj_result

  | Num (num, annot) ->
    begin
      match num with
      | Python2_ast_types.Int n ->
        let storage, int_val =
          store_value ctx annot @@ Integer_literal n
        in
        let wrapping, obj = wrap_int ctx annot int_val in
        storage @ wrapping, obj
      | Python2_ast_types.Float _ ->
        raise @@ Jhupllib_utils.Not_yet_implemented "wrap_float"
    end

  | Str (s, annot) ->
    let storage, int_val =
      store_value ctx annot @@ String_literal s
    in
    let wrapping, obj = wrap_int ctx annot int_val in
    storage @ wrapping, obj

  | Bool (b, annot) ->
    let storage, int_val =
      store_value ctx annot @@ Boolean_literal b
    in
    let wrapping, obj = wrap_int ctx annot int_val in
    storage @ wrapping, obj

  | Builtin _ ->
    raise @@ Jhupllib_utils.Not_yet_implemented "Convert builtin"

  | FunctionVal (args, body, annot) ->
    let gen_arg_binding (list_val, scopename, index, prev) argname =
      let new_scopename = Value_variable(gen_unique_name ctx annot) in
      let argname_value = Value_variable(gen_unique_name ctx annot) in
      let list_access, list_result =
        extract_nth_list_elt ctx annot list_val index
      in
      let directives =
        list_access @
        [
          Let_expression(argname_value, String_literal argname);
          Let_binding_update(new_scopename, scopename, argname_value, list_result);
        ]
      in
      (list_val, new_scopename, index + 1, prev @ directives)
    in

    let lamia_argname = Value_variable(gen_unique_name ctx annot) in
    let empty_scope = Value_variable(gen_unique_name ctx annot) in
    let _, bound_scope, _, arg_bindings =
      List.fold_left gen_arg_binding
        (lamia_argname, empty_scope, 0, [])
        args
    in

    let func_preamble =
      List.map (annotate_directive annot) @@
      [
        Let_alias_value(get_from_parent_scope, get_from_scope);
        Let_alloc(python_scope);
        Let_expression(get_from_scope, get_from_scope_def ctx);
        Let_expression(empty_scope, Empty_binding);
      ] @
      arg_bindings @
      [
        Store(python_scope, bound_scope);
      ]
    in

    let converted_body =
      func_preamble @ map_and_concat (convert_stmt ctx) body
    in
    let lamia_funcval =
      Function_expression([lamia_argname], Block converted_body)
    in

    let funcval_name = Value_variable(gen_unique_name ctx annot) in
    let funcval_loc = Memory_variable(gen_unique_name ctx annot) in
    let actual_bindings =
      List.map (annotate_directive annot) @@
      [
        Let_expression(funcval_name, lamia_funcval);
        Let_alloc(funcval_loc);
        Store(funcval_loc, funcval_name);
      ]
    in
    actual_bindings, funcval_loc

  | Name (id, annot) ->
    lookup ctx annot id
