open Python2_ast_types;;
open Python2_simplified_ast;;

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
let get_call_def name_generator =
  let func_name = name_generator annot in
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

let get_all_builtin_defs name_generator =
  let stmts =
    [
      get_call_def name_generator;
    ]
  in
  Module(stmts, annot)
;;

let parse_all_builtin_defs name_generator starting_uid =
  let defs = get_all_builtin_defs name_generator in
  let module Normalize = Python2_ast_normalizer in
  let ctx = Uid_generation.create_new_uid_context starting_uid in
  Normalize.toggle_short_names true;
  Normalize.normalize_modl ctx defs;
;;
