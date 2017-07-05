(* open Python2_ast_types;; (* TODO: This might cause name conflicts, maybe *) *)
open Python2_pys_interpreter_types;;
open Jhupllib_utils;;

let int_add
    (m : memloc list)
  : micro_instruction list =
  match m with
  | m1::m2::[] ->
    [
      Inert(Micro_memloc(m1));
      Command(GET);
      Inert(Micro_var("*value"));
      Command(RETRIEVE);
      Command(GET);
      Inert(Micro_memloc(Builtin_type_memloc(Int_type)));
      Command(ASSERT 1);
      Inert(Micro_memloc(m2));
      Command(GET);
      Inert(Micro_var("*value"));
      Command(RETRIEVE);
      Command(GET);
      Inert(Micro_memloc(Builtin_type_memloc(Int_type)));
      Inert(Micro_memloc(Builtin_type_memloc(Float_type)));
      Command(ASSERT 2);
      Command(SUM);
    ]
  (* Wrong number of args *)
  | _ -> raise @@ Not_yet_implemented "Wrong number of args"
    (* [Command(ALLOCTYPEERROR); Command(RAISE);] *)
;;

let call_magic m b =
  match b with
  | Builtin_int_add -> int_add m
  | _ -> raise @@ Not_yet_implemented "NYI: Most magics"
;;
