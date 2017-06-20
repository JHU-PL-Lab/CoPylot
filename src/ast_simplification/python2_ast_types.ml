type uid = int
[@@deriving eq, ord, show, to_yojson]
;;

type identifier = string
[@@deriving eq, ord, show, to_yojson]
;;

type 'a annotation =
  {
    uid: uid;
    exception_target: uid option;
    multi: bool;
    body: 'a;
  }
[@@deriving eq, ord, show, to_yojson, to_yojson]
;;

type number =
  | Int of int
  | Float of float
[@@deriving eq, ord, show, to_yojson]
;;

type str =
  | StringLiteral of string
[@@deriving eq, ord, show, to_yojson]
;;

type builtin =
  | Builtin_bool
  | Builtin_slice
  | Builtin_type
  | Builtin_ValueError
  | Builtin_AttributeError
[@@deriving eq, ord, show, to_yojson]
;;
