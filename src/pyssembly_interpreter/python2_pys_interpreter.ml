open Batteries;;
open Python2_normalized_ast;;
open Python2_pys_interpreter_types;;
open Python2_pys_interpreter_utils;;

let step_program (prog : program) : program =
  (* TODO: Use the option monad? *)
  let curr_frame, stack_body = Program_stack.pop prog.stack in
  let active = Stack_frame.active_stmt curr_frame in
  match active with
  | None -> failwith "We can't end functions yet!" (* TODO: pop a stack frame *)
  | Some(s) ->
    begin
      match s.body with
      | Assign (id,
                {body =
                   Literal(l)
                ; _}) ->
        let lval = literal_to_value l in
        let new_heap, new_memloc = allocate_memory prog.heap lval in
        let new_env = bind_var prog.env prog.eta id new_memloc in
        let new_stack = simple_advance_stack curr_frame stack_body in
        { stack = new_stack; heap = new_heap; env = new_env; parents = prog.parents; eta = prog.eta }

      | Assign (id,
                {body =
                   Name(id2)
                ; _}) ->
        let memloc = lookup id2 prog.env prog.parents prog.eta in
        begin
          match memloc with
          | None -> failwith "NYI: Throw NameError"
          | Some(m) ->
            let new_env = bind_var prog.env prog.eta id m in
            let new_stack = simple_advance_stack curr_frame stack_body  in
            { stack = new_stack; heap = prog.heap; env = new_env; parents = prog.parents; eta = prog.eta }
        end
      | _ -> prog
    end
;;

let interpret_program (prog : modl) =
  let Module(stmts, _) = prog in
  let starting_frame = Stack_frame.create (Body.create stmts) in
  let starting_stack = Program_stack.singleton starting_frame in
  let starting_heap = Heap.empty in
  let starting_bindings = Bindings.empty in
  let global_eta = Eta (0) in
  let starting_env = Environment.singleton global_eta starting_bindings in
  let starting_parents = Parents.empty in
  let starting_program =
    {
      stack = starting_stack;
      heap = starting_heap;
      env = starting_env;
      parents = starting_parents;
      eta = global_eta;
    }
  in
  step_program starting_program
;;
