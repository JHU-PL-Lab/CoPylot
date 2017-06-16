open Batteries;;
open Jhupllib_utils;;
open Python2_ast_types;;
open Python2_normalized_ast;;
open Python2_pys_interpreter_types;;
open Python2_pys_interpreter_utils;;

let step_program (prog : program) : program =
  (* TODO: Use the option monad? *)
  let curr_frame, stack_body = Program_stack.pop prog.stack in
  let active = Stack_frame.active_stmt curr_frame in
  match active with
  | None -> raise @@ Not_yet_implemented "NYI: Throw NameError" (* TODO: pop a stack frame *)
  | Some(s) ->
    begin
      match s.body with
      | Assign (id,
                {body =
                   Literal(l)
                ; _}) ->
        let lval = literal_to_value l prog.m in
        let val_heap, val_memloc = allocate_memory prog.heap lval in
        let val_obj = make_literal_obj val_memloc l in
        let new_heap, new_memloc = allocate_memory val_heap @@ Bindings(val_obj) in
        let new_heap2 = bind_var new_heap prog.m id new_memloc in
        let new_stack = simple_advance_stack curr_frame stack_body in
        { stack = new_stack; heap = new_heap2; parents = prog.parents; m = prog.m }

      | Assign (id,
                {body =
                   Name(id2)
                ; _}) ->
        let memloc = lookup id2 prog.heap prog.parents prog.m in
        begin
          match memloc with
          | None -> raise @@ Not_yet_implemented "NYI: Throw NameError"
          | Some(m) ->
            let new_heap = bind_var prog.heap prog.m id m in
            let new_stack = simple_advance_stack curr_frame stack_body  in
            { stack = new_stack; heap = new_heap; parents = prog.parents; m = prog.m }
        end
      | _ -> prog
    end
;;

let interpret_program (prog : modl) =
  let Module(stmts, _) = prog in
  let starting_frame = Stack_frame.create (Body.create stmts) in
  let starting_stack = Program_stack.singleton starting_frame in
  let starting_bindings = Bindings.empty in
  let global_memloc = Memloc(0) in
  let starting_heap =
    Heap.singleton global_memloc @@ Bindings(starting_bindings) in
  let starting_parents = Parents.empty in
  let starting_program =
    {
      stack = starting_stack;
      heap = starting_heap;
      parents = starting_parents;
      m = global_memloc;
    }
  in
  step_program starting_program
;;
