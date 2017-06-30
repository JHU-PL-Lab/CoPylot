open Python2_ast_types;;
open Python2_simplified_ast;;
open Python2_normalization_ctx;;

let annot = Python2_ast.Pos.of_pos Lexing.dummy_pos;;

(* This is the Pyssembly version of the *get_call function, which
   automatically performs lookups of func.__call__ until it finds something that
   is actally callable. The algorithm looks something like this:

   def *get_call(target):
     try:
       while *type(target) is not *method_wrapper_type:
         target = target.__call__
     except AttributeError:
       raise TypeError("Object is not callable")
*)
let get_call_def ctx =
  let target = gen_unique_name ctx annot in
  let type_test =
    Compare(
      Call(Builtin(Builtin_type, annot),
           [Name(target, annot)],
           annot),
      [IsNot],
      [Builtin(Builtin_method_wrapper_type, annot)],
      annot)
  in
  (* This might be an infinite loop if the user is doing shenanigans *)
  let getcall_loop = While(
      type_test,
      [
        Assign(target,
               Attribute(
                 Name(target, annot),
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
                (* FIXME: This string should be dynamically constructed to
                   say what the original object type was *)
                [Str(StringLiteral("Object is not callable"), annot)],
                annot),
              annot);
          ],
          annot)],
      annot)
  in
  Assign("*get_call",
         FunctionVal(
           [target],
           [
             overall_try;
             Return(Name(target, annot),
                    annot);
           ],
           annot),
         annot)
;;

(* Looks up the inheritance chain of obj in the appropriate order, and returns
   the value of the attribute "mem" for the first one that it sees. If it
   never sees one, it raises an AttributeError *)
(* TODO: When we implement classes we actually need to get the mro and walk up
   it. For the moment we don't have inheritance, so this is just a simple
   dictionary lookup. *)
let get_attribute_def ctx =
  let obj = gen_unique_name ctx annot in
  let mem = gen_unique_name ctx annot in
  Assign("*get_attribute",
         FunctionVal(
           [obj; mem],
           [
             Return(ImmediateAttribute(Name(obj, annot), mem, annot),
                    annot);
           ],
           annot),
         annot)
;;

let get_all_builtin_defs ctx =
  let stmts =
    [
      get_call_def ctx;
      get_attribute_def ctx;
    ]
  in
  Module(stmts, annot)
;;

let parse_all_builtin_defs starting_uid =
  let ctx = create_new_normalization_ctx starting_uid 0 "$builtin" in
  let defs = get_all_builtin_defs ctx in
  Python2_ast_normalizer.normalize_modl ctx defs;
;;
