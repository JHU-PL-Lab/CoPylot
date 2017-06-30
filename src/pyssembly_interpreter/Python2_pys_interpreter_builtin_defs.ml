open Python2_ast_types;;
open Python2_simplified_ast;;
open Python2_normalization_ctx;;

let annot = Python2_ast.Pos.of_pos Lexing.dummy_pos;;

(* This is the Pyssembly version of the *get_call function, which
   automatically performs lookups of func.__call__ until it finds something that
   is actally callable. The algorithm looks something like this:

   def *get_call(func_name):
     try:
       while *type(func_name) is not *method_wrapper_type:
         func_name = func_name.__call__
     except AttributeError:
       raise TypeError("Object is not callable")
*)
(* FIXME: The TypeError string should be dynamically constructed to say what the
   original object type was *)
let get_call_def ctx =
  let func_name = gen_unique_name ctx annot in
  let type_test =
    Compare(
      Call(Builtin(Builtin_type, annot),
                      [Name(func_name, annot)],
                      annot),
      [IsNot],
      [Builtin(Builtin_method_wrapper_type, annot)],
      annot)
  in
  (* This might be an infinite loop if the user is doing shenanigans *)
  let getcall_loop = While(
      type_test,
      [
        Assign(func_name,
                          Attribute(
                            Name(func_name, annot),
                            "__call__",
                            annot),
                          annot);
      ],
      annot)
  in
  let overall_try = TryExcept(
      [getcall_loop],
      [ExceptHandler(
          Some(Builtin(Builtin_AttributeError, annot)),
          None,
          [
            Raise(
              Call(
                Builtin(Builtin_TypeError, annot),
                [Str(StringLiteral("Object is not callable"), annot)],
                annot),
              annot);
          ],
          annot)],
      annot)
  in
  Assign("*get_call",
                    FunctionVal(
                      [func_name],
                      [
                        overall_try;
                        Return(Name(func_name, annot),
                                          annot);
                      ], annot),
                    annot)
;;

let get_all_builtin_defs ctx =
  let stmts =
    [
      get_call_def ctx;
    ]
  in
  Module(stmts, annot)
;;

let parse_all_builtin_defs starting_uid =
  let ctx = create_new_normalization_ctx starting_uid 0 "$builtin" in
  let defs = get_all_builtin_defs ctx in
  Python2_ast_normalizer.normalize_modl ctx defs;
;;
