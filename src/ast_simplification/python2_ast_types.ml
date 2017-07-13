type uid = int
[@@deriving eq, ord, show, to_yojson]
;;

type identifier = string
[@@deriving eq, ord, show, to_yojson]
;;

type 'a annotation =
  {
    annot: Python2_ast.Pos.t;
    body: 'a;
  }
  (* [@@deriving eq, ord, show, to_yojson] *)
;;
(* TODO: add definitions to Pos sig so we don't have to do this manually.
   For now we just ignore the annot and operate on the body only. *)
let pp_annotation pp_a fmt a = pp_a fmt a.body;;
let equal_annotation eq_a a1 a2 = eq_a a1.body a2.body;;
let compare_annotation cmp_a a1 a2 = cmp_a a1.body a2.body;;
let annotation_to_yojson a_to_yojson a = a_to_yojson a.body;;

type number =
  | Int of int
  | Float of float
[@@deriving eq, ord, show, to_yojson]
;;

type builtin =
  | Builtin_bool
  | Builtin_slice
  | Builtin_type
  | Builtin_method_wrapper_type
  | Builtin_ValueError
  | Builtin_AttributeError
  | Builtin_TypeError
[@@deriving eq, ord, show, to_yojson]
;;
