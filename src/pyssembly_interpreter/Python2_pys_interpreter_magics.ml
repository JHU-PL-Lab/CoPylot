open Python2_ast_types;; (* TODO: This might cause name conflicts, maybe *)
open Python2_pys_interpreter_types;;
module MIS = Micro_instruction_stack;;

(* A builtin function should obey the following rules: it takes a heap,
   a Micro_instruction_stack, and a memloc list, and returns a micro_instruction
   list. The list should evaluate to a single value, which is the return value
   of the function *)

let int_add
    (heap: Heap.t)
    (micro: Micro_instruction_stack.t)
    (m : memloc list)
  : MIS.t =
  match m with
  | m1::m2::[] ->
    begin
    let v1 = Heap.get_value m1 heap in
    let v2 = Heap.get_value m2 heap in
    match v1 with
    | Num(Int n1) ->
      begin
        match v2 with
        | Num(Int n2) ->
          let sum = n1 + n2 in
          MIS.insert micro @@
          MIS.create [Inert(Micro_value(Num(Int(sum))))]
        | Num(Float n2) ->
          let sum = (float_of_int n1) +. n2 in
          MIS.insert micro @@
          MIS.create [Inert(Micro_value(Num(Float(sum))))]
        | _ ->
          (* Second type not numeric *)
          MIS.create [Command(ALLOCTYPEERROR); Command(RAISE);]
      end
    | _ -> failwith "Int add called with first argument not an int!"
  end
      (* Wrong number of args *)
  | _ -> MIS.create [Command(ALLOCTYPEERROR); Command(RAISE);]
;;

let call_magic heap micro m b =
  match b with
  | Builtin_int_add -> int_add heap micro m
  | _ -> raise @@ Jhupllib_utils.Not_yet_implemented "NYI: Most magics"
;;
