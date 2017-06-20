open Batteries;;
open Jhupllib_utils;;
open Python2_ast_types;;
module Normalized = Python2_normalized_ast;;
open Normalized;;
open Python2_pys_interpreter_types;;
open Python2_pys_interpreter_builtin_objects;;
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
        let memloc = lookup_or_error prog.heap prog.parents prog.m id2 in
        let new_heap = bind_var prog.heap prog.m id memloc in
        let new_stack = simple_advance_stack curr_frame stack_body  in
        { stack = new_stack; heap = new_heap; parents = prog.parents; m = prog.m }

      | Assign (id, {body =
                       Normalized.List(elts)
                    ; _ }) ->
        let memlocs =
          List.map (lookup_or_error prog.heap prog.parents prog.m) elts
        in
        let lval = ListVal(memlocs) in
        let val_heap, val_memloc = allocate_memory prog.heap lval in
        let val_obj = make_list_obj val_memloc in
        let obj_heap, obj_memloc =
          allocate_memory val_heap @@ Bindings(val_obj)
        in
        let new_heap = bind_var obj_heap prog.m id obj_memloc in
        let new_stack = simple_advance_stack curr_frame stack_body in
        { stack = new_stack; heap = new_heap; parents = prog.parents; m = prog.m }

      | Assign (id, {body =
                       Normalized.Tuple(elts)
                    ; _ }) ->
        let memlocs =
          List.map (lookup_or_error prog.heap prog.parents prog.m) elts
        in
        let lval = TupleVal(memlocs) in
        let val_heap, val_memloc = allocate_memory prog.heap lval in
        let val_obj = make_tuple_obj val_memloc in
        let obj_heap, obj_memloc =
          allocate_memory val_heap @@ Bindings(val_obj)
        in
        let new_heap = bind_var obj_heap prog.m id obj_memloc in
        let new_stack = simple_advance_stack curr_frame stack_body in
        { stack = new_stack; heap = new_heap; parents = prog.parents; m = prog.m }

      (* | Assign (id, {body =
                       Normalized.Call(func, args)
                    ; _ }) ->
         let func_memloc = lookup_or_error prog.heap prog.parents prog.m func in
         let func_obj = Heap.get_value func_memloc prog.heap in
         let arg_memlocs =
          List.map (lookup_or_error prog.heap prog.parents prog.m) elts
         in
         let lval = TupleVal(memlocs) in
         let val_heap, val_memloc = allocate_memory prog.heap lval in
         let val_obj = make_tuple_obj val_memloc in
         let obj_heap, obj_memloc =
          allocate_memory val_heap @@ Bindings(val_obj)
         in
         let new_heap = bind_var obj_heap prog.m id obj_memloc in
         let new_stack = simple_advance_stack curr_frame stack_body in
         { stack = new_stack; heap = new_heap; parents = prog.parents; m = prog.m } *)

      | Raise (id) ->
        let id_loc = lookup prog.heap prog.parents prog.m id in
        begin
          match id_loc with
          | None -> throw_exception prog @@ Builtin_memloc(Builtin_NameError) (* TODO: Call constructor *)
          | Some(m) -> throw_exception prog m
        end

      | Catch _ ->
        failwith "SyntaxError: Reached catch statement with no raised value"

      | Pass ->
        let new_stack = simple_advance_stack curr_frame stack_body in
        { stack = new_stack; heap = prog.heap; parents = prog.parents; m = prog.m }

      | Return (id) ->
        let id_loc = lookup_or_error prog.heap prog.parents prog.m id in
        let prev_frame, rest_of_stack = Program_stack.pop stack_body in
        let callsite_binding_id =
          begin
            let callsite = Stack_frame.active_stmt prev_frame in
            match callsite with
            | Some({body = Assign(id, {body = Call(_); _}); _}) -> id
            | _ -> failwith "Did not see callsite on return from function"
          end in
        let new_heap = bind_var prog.heap prog.m callsite_binding_id id_loc in
        let prev_m =
          begin
            let parent = Parents.get_parent prog.m prog.parents in
            match parent with
            | None -> failwith "No parent scope on returning from function."
            | Some (p) -> p
          end
        in
        let new_stack = simple_advance_stack prev_frame rest_of_stack in
        { stack = new_stack; heap = new_heap; parents = prog.parents; m = prev_m}

      | Goto (uid) ->
        let new_frame = Stack_frame.advance curr_frame @@ Uid(uid) in
        let new_stack = Program_stack.push prog.stack new_frame in
        { stack = new_stack; heap = prog.heap; parents = prog.parents; m = prog.m }

      | GotoIfNot (id, uid) ->
        let id_memloc = lookup_or_error prog.heap prog.parents prog.m id in
        let id_val = get_obj_value prog.heap id_memloc in
        let new_stack =
          begin
            match id_val with
            | None -> failwith "GotoIfNot passed on object with no *value field"
            | Some(Bool(false)) ->
              let new_frame = Stack_frame.advance curr_frame @@ Uid(uid) in
              Program_stack.push prog.stack new_frame
            | Some(Bool(true)) ->
              simple_advance_stack curr_frame stack_body
            | _ -> failwith "GotoIfNot passed non-boolean object"
          end
        in
        { stack = new_stack; heap = prog.heap; parents = prog.parents; m = prog.m }

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
