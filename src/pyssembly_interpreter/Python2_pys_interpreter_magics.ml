open Python2_ast_types;; (* TODO: This might cause name conflicts, maybe *)
open Python2_pys_interpreter_types;;
open Python2_pys_interpreter_utils;;

(* A builtin function should obey the following rules: it takes a heap,
   a Micro_instruction_stack, and a memloc list, and returns a bool and a
   micro_instruction list. The bool represents success: if true, the MI list
   should evaluate to a single value, which is the return value of the function.
   If false, the MI list should raise an exception. *)

let int_add
    (heap: Heap.t)
    (m : memloc list)
  : bool * micro_instruction list =
  match m with
  | m1::m2::[] ->
    begin
      let v1 =
        retrieve_binding_or_fail heap m1
        |> Bindings.get_memloc "*value"
          (* TODO: When we implement classes, this should raise a TypeError instead of failing *)
        |> (fun o -> extract_option_or_fail o "First argument to int add did not have *value field!")
        |> (fun m -> Heap.get_value m heap)
      in
      match v1 with
      | Num(Int n1) ->
        begin
          let v2 =
            retrieve_binding_or_fail heap m2
            |> Bindings.get_memloc "*value"
          in

          match v2 with
          | None -> (* Second type not numeric *)
            false, [Command(ALLOCTYPEERROR); Command(RAISE);]
          | Some m ->
            match Heap.get_value m heap with
            | Num(Int n2) ->
              let sum = n1 + n2 in
              true, [Inert(Micro_value(Num(Int(sum))))]
            | Num(Float n2) ->
              let sum = (float_of_int n1) +. n2 in
              true, [Inert(Micro_value(Num(Float(sum))))]
            | _ ->
              (* Second type not numeric *)
              false, [Command(ALLOCTYPEERROR); Command(RAISE);]
        end
      | _ ->
        let whatwegot = Jhupllib_pp_utils.pp_to_string pp_value v1 in
        failwith @@ "Int add called with first argument not an int! Got " ^ whatwegot ^ " instead!"
    end
  (* Wrong number of args *)
  | _ -> false, [Command(ALLOCTYPEERROR); Command(RAISE);]
;;

let call_magic heap m b =
  match b with
  | Builtin_int_add -> int_add heap m
  | _ -> raise @@ Jhupllib_utils.Not_yet_implemented "NYI: Most magics"
;;
