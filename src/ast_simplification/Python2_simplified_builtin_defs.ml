open Python2_ast_types;;
module Simplified = Python2_simplified_ast;;

let annot = Python2_ast.Pos.of_pos Lexing.dummy_pos;;

(* This is the Simplified Pyssembly version of the *get_call function, which
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
    Simplified.Compare(
      Simplified.Call(Simplified.Builtin(Builtin_type, annot),
                      [Simplified.Name(func_name, annot)],
                      annot),
      [Simplified.IsNot],
      [Simplified.Builtin(Builtin_method_wrapper_type, annot)],
      annot)
  in
  (* This might be an infinite loop if the user is doing shenanigans *)
  let getcall_loop = Simplified.While(
      type_test,
      [
        Simplified.Assign(func_name,
                          Simplified.Attribute(
                            Simplified.Name(func_name, annot),
                            "__call__",
                            annot),
                          annot);
      ],
      annot)
  in
  let overall_try = Simplified.TryExcept(
      [getcall_loop],
      [Simplified.ExceptHandler(
          Some(Simplified.Builtin(Builtin_AttributeError, annot)),
          None,
          [
            Simplified.Raise(
              Simplified.Call(
                Simplified.Builtin(Builtin_TypeError, annot),
                [Simplified.Str(StringLiteral("Object is not callable"), annot)],
                annot),
              annot);
          ],
          annot)],
      annot)
  in
  Simplified.Assign("*get_call",
                    Simplified.FunctionVal(
                      [func_name],
                      [
                        overall_try;
                        Simplified.Return(Simplified.Name(func_name, annot),
                                          annot);
                      ], annot),
                    annot)
;;
