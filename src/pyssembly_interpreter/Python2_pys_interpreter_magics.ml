open Python2_ast_types;; (* TODO: This might cause name conflicts, maybe *)
open Python2_pys_interpreter_types;;
open Jhupllib_utils;;

let type_func
    (m : memloc list)
  : micro_instruction list =
  match m with
  | m1::[] ->
    [
      Inert(Micro_memloc(m1));
      Command(GET);
      Inert(Micro_value(Str(StringLiteral "__class__")));
      Command(RETRIEVE);
    ]
  (* Wrong number of args *)
  | _ -> failwith "Wrong number of args to type_func"
;;

let bool_func
    (m : memloc list)
  : micro_instruction list =
  match m with
  | m1::[] ->
    [
      Inert(Micro_memloc(m1));
      Command(GET);
      Inert(Micro_value(Str(StringLiteral "*value")));
      Command(RETRIEVE);
      Command(GET);
      Command(BOOL);
    ]
  (* Wrong number of args *)
  | _ -> failwith "Wrong number of args to bool_func"
;;

let int_add
    (m : memloc list)
  : micro_instruction list =
  match m with
  | m1::m2::[] ->
    [
      Inert(Micro_memloc(m1));
      Command(GET);
      Inert(Micro_value(Str(StringLiteral "*value")));
      Command(RETRIEVE);
      Command(GET);
      Inert(Micro_memloc(Builtin_type_memloc(Int_type)));
      Command(ASSERT 1);
      Inert(Micro_memloc(m2));
      Command(GET);
      Inert(Micro_value(Str(StringLiteral "*value")));
      Command(RETRIEVE);
      Command(GET);
      Inert(Micro_memloc(Builtin_type_memloc(Int_type)));
      Inert(Micro_memloc(Builtin_type_memloc(Float_type)));
      Command(ASSERT 2);
      Command(SUM);
    ]
  (* Wrong number of args *)
  | _ -> failwith "Wrong number of args to int_add"
;;

let int_neg
    (m : memloc list)
  : micro_instruction list =
  match m with
  | m1::[] ->
    [
      Inert(Micro_memloc(m1));
      Command(GET);
      Inert(Micro_value(Str(StringLiteral "*value")));
      Command(RETRIEVE);
      Command(GET);
      Inert(Micro_memloc(Builtin_type_memloc(Int_type)));
      Command(ASSERT 1);
      Command(NEG);
    ]
  (* Wrong number of args *)
  | _ -> failwith "Wrong number of args to int_neg"
;;

let str_add
    (m : memloc list)
  : micro_instruction list =
  match m with
  | m1::m2::[] ->
    [
      Inert(Micro_memloc(m1));
      Command(GET);
      Inert(Micro_value(Str(StringLiteral "*value")));
      Command(RETRIEVE);
      Command(GET);
      Inert(Micro_memloc(Builtin_type_memloc(String_type)));
      Command(ASSERT 1);
      Inert(Micro_memloc(m2));
      Command(GET);
      Inert(Micro_value(Str(StringLiteral "*value")));
      Command(RETRIEVE);
      Command(GET);
      Inert(Micro_memloc(Builtin_type_memloc(String_type)));
      Command(ASSERT 1);
      Command(STRCONCAT);
    ]
  (* Wrong number of args *)
  | _ -> failwith "Wrong number of args to str_add"
;;

let str_contains
    (m : memloc list)
  : micro_instruction list =
  match m with
  | m1::m2::[] ->
    [
      Inert(Micro_memloc(m1));
      Command(GET);
      Inert(Micro_value(Str(StringLiteral "*value")));
      Command(RETRIEVE);
      Command(GET);
      Inert(Micro_memloc(Builtin_type_memloc(String_type)));
      Command(ASSERT 1);
      Inert(Micro_memloc(m2));
      Command(GET);
      Inert(Micro_value(Str(StringLiteral "*value")));
      Command(RETRIEVE);
      Command(GET);
      Inert(Micro_memloc(Builtin_type_memloc(String_type)));
      Command(ASSERT 1);
      Command(STRCONTAINS);
    ]
  (* Wrong number of args *)
  | _ -> failwith "Wrong number of args to str_contains"
;;

let list_getitem
    (m : memloc list)
  : micro_instruction list =
  match m with
  | m1::m2::[] ->
    [
      Inert(Micro_memloc(m1));
      Command(GET);
      Inert(Micro_value(Str(StringLiteral "*value")));
      Command(RETRIEVE);
      Command(GET);
      Inert(Micro_memloc(Builtin_type_memloc(List_type)));
      Command(ASSERT 1);
      Inert(Micro_memloc(m2));
      Command(GET);
      Inert(Micro_value(Str(StringLiteral "*value")));
      Command(RETRIEVE);
      Command(GET);
      Inert(Micro_memloc(Builtin_type_memloc(Int_type)));
      Command(ASSERT 1);
      Command(GETITEM);
    ]
  (* Wrong number of args *)
  | _ -> failwith "Wrong number of args to list_getitem"
;;

let call_magic m b =
  (* TODO: check number of args *)
  (* [Command(ALLOCTYPEERROR); Command(RAISE);] *)
  match b with
  | Builtin_type_func -> type_func m
  | Builtin_bool -> bool_func m
  | Builtin_int_add -> int_add m
  | Builtin_int_neg -> int_neg m
  | Builtin_string_add -> str_add m
  | Builtin_string_contains -> str_contains m
  | Builtin_list_getitem -> list_getitem m
  | _ -> raise @@ Not_yet_implemented "call_magic for most magics"
;;

let returns_memloc = function
  | Builtin_type_func -> true
  | Builtin_bool -> false
  | Builtin_int_add -> false
  | Builtin_int_neg -> false
  | Builtin_string_add -> false
  | Builtin_string_contains -> false
  | Builtin_list_getitem -> true
  | _ -> raise @@ Not_yet_implemented "returns_memloc for most magics"
