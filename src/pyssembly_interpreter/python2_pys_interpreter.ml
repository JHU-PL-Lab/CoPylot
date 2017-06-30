open Batteries;;
open Jhupllib_utils;;
open Python2_ast_types;;
module Normalized = Python2_normalized_ast;;
open Normalized;;
open Python2_pys_interpreter_types;;
(* open Python2_pys_interpreter_builtin_objects;; *)
open Python2_pys_interpreter_utils;;

let execute_micro_command (prog : program_state) (ctx : program_context)
  : program_state =
  let module MIS = Micro_instruction_stack in

  let command, rest_of_stack = MIS.pop_first_command prog.micro in
  match command with
  (* STORE command; takes a value and binds it to a fresh memory location on
     the heap. Returns the fresh memory location. *)
  | STORE ->
    let val_to_store, popped_stack = pop_value_or_fail rest_of_stack "STORE" in
    let fresh_memloc = Heap.get_new_memloc prog.heap in
    let new_heap = Heap.update_binding fresh_memloc val_to_store prog.heap in
    let new_micro = MIS.insert popped_stack @@
      MIS.create [Inert(Micro_memloc(fresh_memloc))]
    in
    {
      prog with
      micro = new_micro;
      heap = new_heap;
    }

  (* WRAP command: takes a memory location. If that memloc points to a value, it
     wraps the value in the appropriate object type, and returns that object.
     If the memloc points to an already-existing object, it simply returns that
     object. *)
  | WRAP ->
    let v, popped_stack = pop_value_or_fail rest_of_stack "WRAP" in
    let m, popped_stack2 = pop_memloc_or_fail popped_stack "WRAP" in
    let obj_val = wrap_value m v in
    let new_micro = MIS.insert popped_stack2 @@
      MIS.create [Inert(Micro_value(obj_val));]
    in
    { prog with micro = new_micro; }
    (* BIND command: takes an identifier and a memloc. Binds the memloc to the id,
       and returns nothing *)
  | BIND ->
    let x, popped_stack1 = pop_var_or_fail rest_of_stack "BIND" in
    let m, popped_stack2 = pop_memloc_or_fail popped_stack1 "BIND"in
    let curr_eta = Stack_frame.get_eta (Program_stack.top prog.stack) in
    let bindings =
      retrieve_binding_or_fail prog.heap curr_eta
    in
    let new_bindings = Bindings.update_binding x m bindings in
    let new_heap =
      Heap.update_binding curr_eta (Bindings(new_bindings)) prog.heap
    in
    {
      prog with
      micro = popped_stack2;
      heap = new_heap;
    }

  (* ADVANCE command: takes no arguments, and advances the current stack frame's
     instruction pointer to the next label. Note that this advances to the next
     statement lexically, and hence should not be used with GOTOs *)
  | ADVANCE ->
    let curr_frame, stack_body = Program_stack.pop prog.stack in
    let next_uid = Body.get_next_uid ctx.program @@ Stack_frame.get_active_uid curr_frame in
    begin
      match next_uid with
      | Some(u) ->
        let next_frame = Stack_frame.update_active_uid curr_frame u in
        let new_stack = Program_stack.push stack_body next_frame in
        { prog with micro = rest_of_stack; stack = new_stack }

      | None ->
        let new_micro_list =
          if Program_stack.is_empty stack_body then
            (* End of program *)
            [ Command(POP) ]
          else
            (* End of function: treat the same as "return None" *)
            let caller = Program_stack.top stack_body in
            let caller_active =
              Body.get_stmt ctx.program @@ Stack_frame.get_active_uid caller
            in
            let x =
              match caller_active with
              | Some({body = Assign(id, _); _}) -> id
              | _ -> failwith "Did not see assign on return from call!"
            in
            [
              Command(POP);
              Inert(Micro_memloc(None_memloc));
              Inert(Micro_var(x));
              Command(BIND);
              Command(ADVANCE);
            ]
        in
        let new_micro =
          MIS.insert rest_of_stack @@ MIS.create new_micro_list
        in
        { prog with micro = new_micro }
    end

  (* POP command: Takes no arguments. Removes the current stack frame, and
     updates eta accordingly. *)
  | POP ->
    let _, stack_body = Program_stack.pop prog.stack in
    { prog with micro = rest_of_stack; stack = stack_body }

  (* PUSH command: Takes a memloc as an argument. Creates a new stack frame with
     its body and that memloc, and pushes that frame to the stack. *)
  | PUSH (uid) ->
    let eta, popped_micro = pop_memloc_or_fail rest_of_stack "PUSH" in

    let new_eta = Heap.get_new_memloc prog.heap in
    let new_scope = Bindings.singleton "*parent" eta in
    let new_heap = Heap.update_binding new_eta (Bindings(new_scope)) prog.heap in

    let new_frame = Stack_frame.create new_eta uid in
    let new_stack = Program_stack.push prog.stack new_frame in
    { micro = popped_micro; stack = new_stack; heap = new_heap }

  (* LOOKUP command: takes an identifier, and returns the memory address bound to
     that variable in the closest scope in which it is bound. Raises a NameError
     if the variable is unbound. *)
  | LOOKUP ->
    let target, popped_stack = pop_var_or_fail rest_of_stack "LOOKUP" in
    let rec lookup
        (eta : memloc)
        (id: identifier)
      : memloc option =
      let bindings = retrieve_binding_or_fail prog.heap eta in
      let m = Bindings.get_memloc id bindings in
      match m with
      | Some _ -> m
      | None ->
        let parent = Bindings.get_memloc "*parent" bindings in
        match parent with
        | None -> None
        | Some(p) -> lookup p id
    in
    let curr_eta = Stack_frame.get_eta (Program_stack.top prog.stack) in
    let lookup_result = lookup curr_eta target in
    let new_micro =
      match lookup_result with
      | None -> (* Lookup failed, raise a NameError *)
        MIS.create [ Command(ALLOCNAMEERROR); Command(RAISE); ]

      | Some(m) ->
        MIS.insert popped_stack @@ MIS.create [ Inert(Micro_memloc(m)); ]
    in
    { prog with micro = new_micro }

  (* GETVALUE command: takes a memory location, and returns the value at that
     address *)
  | GET ->
    let m, popped_stack = pop_memloc_or_fail rest_of_stack "GETVALUE" in
    let v = Heap.get_value m prog.heap in
    let new_micro = MIS.insert popped_stack @@
      MIS.create [Inert(Micro_value(v))]
    in
    { prog with micro = new_micro }

  (* ASSIGN command: Takes a raw value and an identifier, and generates the
     necessary commands to store that value in memory and bind it to the
     identifier. *)
  | ASSIGN ->
    let x, popped_stack = pop_var_or_fail rest_of_stack "ASSIGN" in
    let v, popped_stack2 = pop_value_or_fail popped_stack "ASSIGN" in
    let new_micro = MIS.insert popped_stack2 @@
      MIS.create
        [
          Inert(Micro_value(v));
          Command(STORE);
          Inert(Micro_value(v));
          Command(WRAP);
          Command(STORE);
          Inert(Micro_var(x));
          Command(BIND);
        ]
    in
    { prog with micro = new_micro }

  (* LIST command: expects there to be size memlocs above it in the stack. Creates
     a list value containing those memlocs, with memlocs closer to the LIST command
     being further down the list *)
  | LIST (size) ->
    let elts, popped_micro = pop_n_memlocs size [] rest_of_stack "LIST" in
    let new_micro = MIS.insert popped_micro @@
      MIS.create [ Inert(Micro_value(ListVal(elts))); ]
    in
    { prog with micro = new_micro }

  (* TUPLE command: Exactly the same as LIST, but returns a tuple *)
  | TUPLE (size) ->
    let elts, popped_micro = pop_n_memlocs size [] rest_of_stack "TUPLE" in
    let new_micro = MIS.insert popped_micro @@
      MIS.create [ Inert(Micro_value(TupleVal(elts))); ]
    in
    { prog with micro = new_micro }

  (* RAISE command: takes no arguments (though there should be a memloc
     immediately before it for future instructions to use). It examines the
     currently active statement. If it has no exception label, we pop a stack
     frame. Otherwise, if the exception statement points to a catch statement,
     we move to that catch and execute it. *)
  | RAISE ->
    let stack_top, stack_body = Program_stack.pop prog.stack in
    let active = get_active_or_fail stack_top ctx in
    begin
      match active.exception_target with
      | None -> (* No exception target, pop a stack frame *)
        let new_micro = MIS.insert rest_of_stack @@
          MIS.create [ Command(POP); Command(RAISE); ]
        in
        { prog with micro = new_micro }

      | Some(uid) ->
        let catch_stmt = Body.get_stmt ctx.program uid in
        match catch_stmt with
        | Some({body = Catch (x);_}) ->
          let new_micro = MIS.insert rest_of_stack @@
            MIS.create [ Inert(Micro_var(x)); Command(BIND); Command(ADVANCE); ]
          in
          let catch_frame = Stack_frame.update_active_uid stack_top uid in
          let new_stack = Program_stack.push stack_body catch_frame in
          { prog with micro = new_micro; stack = new_stack; }

        | _ -> failwith "Exception label did not point to a catch in the same scope!"
    end

  (* GOTO command: Takes no arguments, and moves the instruction pointer on the
     current stack frame to the specified label. *)
  | GOTO uid ->
    let curr_frame, stack_body = Program_stack.pop prog.stack in
    let next_frame = Stack_frame.update_active_uid curr_frame @@ uid in
    let new_stack = Program_stack.push stack_body next_frame in
    { prog with micro = rest_of_stack; stack = new_stack }

  (* GOTOIFNOT command: Takes a memloc, and moves the instruction pointer on the
     current stack frame to the specified label if the value at that memory
     address is "false". If the value is "true" it does nothing. If the value
     is any non-boolean, the program fails.*)
  | GOTOIFNOT uid ->
    let m, popped_stack = pop_memloc_or_fail rest_of_stack "GOTOIFNOT" in
    let value = Heap.get_value m prog.heap in
    let new_micro_list =
      match value with
      | Bool(true)  -> [ Command(ADVANCE); ]
      | Bool(false) -> [ Command(GOTO(uid)); ]
      | _ -> failwith "GOTOIFNOT not given a boolean value!"
    in
    let new_micro = MIS.insert popped_stack @@ MIS.create new_micro_list in
    { prog with micro = new_micro }

  (* CONVERT command: expects to see numargs+1 memory locations before it. If
     n+1th one points to a function value, we do nothing and replace ourselves
     with a call command. If points to a method value, we turn it into a
     function value by adding the self arg to the micro-stack, and then replace
     ourselves with a call command. *)
  | CONVERT numargs ->
    let arg_locs, popped_micro = pop_n_memlocs numargs [] rest_of_stack "CONVERT" in
    let func_val, popped_micro2 = pop_value_or_fail popped_micro "CONVERT" in
    let new_micro =
      match func_val with
      | Function _ ->
        (* We only change the command, so we can re-use the stack that only
           had that popped *)
        MIS.insert rest_of_stack @@
        MIS.create [Command(CALL(numargs))]

      | Method (arg, func) ->
        let arglist = List.map (fun m -> Inert(Micro_memloc(m))) arg_locs in
        MIS.insert popped_micro2 @@
        MIS.create @@
        [
          Inert(Micro_value(Function(func)));
          Inert(Micro_memloc(arg));
        ] @
        arglist @
        [Command(CALL(numargs+1))]

      | _ -> failwith "CONVERT not given a function or method!"
    in
    { prog with micro = new_micro }

  (* CALL command: Expects to see numargs+1 memory locations before it, where the
     furthest one points to a function or method value to be called. Issues
     instructions to push a stack frame corresponding to that function, then
     binds all the arguments appropriately. *)
  | CALL numargs ->
    let arg_locs, popped_micro = pop_n_memlocs numargs [] rest_of_stack "CALL" in
    (* TODO: Current spec says this should be a value, not a memloc. May or may
       not get changed; fix when we've decided *)
    let func_loc, popped_micro2 = pop_memloc_or_fail popped_micro "CALL" in
    let value = Heap.get_value func_loc prog.heap in
    let new_micro =
      match value with
      | Function (User_func(eta, args, body)) ->
        if List.length args <> numargs then
          MIS.create [ Command(ALLOCTYPEERROR); Command(RAISE); ]
        else
          let binds = List.concat @@
            List.map2 (fun m x ->
                [ Inert(Micro_memloc(m)); Inert(Micro_var(x)); Command(BIND); ]
              )
              arg_locs args
          in
          MIS.insert popped_micro2 @@ MIS.create @@
          [ Inert(Micro_memloc(eta)); Command(PUSH body); ] @ binds

      | Function (Builtin_func _) ->
        raise @@ Not_yet_implemented "Builtin function"

      | _ -> failwith "Can only CALL a function."
    in
    { prog with micro = new_micro; }

  (* RETRIEVE command: Takes a memloc which points to an object, and an
     identifier. Retrieves the object field corresponding to that identifier. *)
  | RETRIEVE ->
    let member, popped_stack = pop_var_or_fail rest_of_stack "RETRIEVE" in
    let obj_memloc, popped_stack2 = pop_memloc_or_fail popped_stack "RETRIEVE" in
    let obj = retrieve_binding_or_fail prog.heap obj_memloc in
    let attr = Bindings.get_memloc member obj in
    let new_micro =
      match attr with
      | None ->
        MIS.create [ Command(ALLOCATTRIBUTEERROR); Command (RAISE); ]
      | Some(m) ->
        MIS.insert popped_stack2 @@
        MIS.create [ Inert(Micro_memloc(m)); ]
    in
    { prog with micro = new_micro; }

  | ALLOCNAMEERROR -> raise @@ Not_yet_implemented "ALLOCNAMEERROR"
  | ALLOCTYPEERROR -> raise @@ Not_yet_implemented "ALLOCTYPEERROR"
  | ALLOCATTRIBUTEERROR -> raise @@ Not_yet_implemented "ALLOCATTRIBUTEERROR"
;;

let execute_stmt (prog : program_state) (ctx: program_context): program_state =
  (* Each statement generates a list of micro-instructions, which are then
     executed. This should not be called if the current micro-instruction list
     is nonempty! *)
  let new_micro_list =
    let curr_frame, stack_body = Program_stack.pop prog.stack in
    let stmt = get_active_or_fail curr_frame ctx in
    match stmt.body with
    (* Assignment from literal (also includes function values) *)
    | Assign(x, {body = Literal(l); _}) ->
      let curr_eta = Stack_frame.get_eta (Program_stack.top prog.stack) in
      let lval = literal_to_value l curr_eta in
      [
        Inert(Micro_value(lval));
        Inert(Micro_var(x));
        Command(ASSIGN);
        Command(ADVANCE);
      ]

    (* Variable Aliasing *)
    | Assign(x1, {body = Name(x2); _}) ->
      [
        Inert(Micro_var(x2));
        Command(LOOKUP);
        Inert(Micro_var(x1));
        Command(BIND);
        Command(ADVANCE);
      ]

    (* Assign from list *)
    | Assign(x, {body = List(elts); _}) ->
      let lookups = List.concat @@ List.map
          (fun id -> [ Inert(Micro_var(id)); Command(LOOKUP); ])
          elts
      in
      lookups @
      [
        Command(LIST(List.length elts));
        Inert(Micro_var(x));
        Command(ASSIGN);
        Command(ADVANCE);
      ]

    (* Assign from tuple *)
    | Assign(x, {body = Tuple(elts); _}) ->
      let lookups = List.concat @@ List.map
          (fun id -> [ Inert(Micro_var(id)); Command(LOOKUP); ])
          elts
      in
      lookups @
      [
        Command(TUPLE(List.length elts));
        Inert(Micro_var(x));
        Command(ASSIGN);
        Command(ADVANCE);
      ]

    (* Object attribute *)
    | Assign(x, {body = Attribute(x1, x2); _}) ->
      [
        Inert(Micro_var(x1));
        Command(LOOKUP);
        Inert(Micro_var(x2));
        Command(RETRIEVE);
        Inert(Micro_var(x));
        Command(BIND);
        Command(ADVANCE);
      ]

    (* Function call *)
    | Assign(_, {body = Call(x0, args); _}) ->
      let arg_lookups = List.concat @@ List.map
          (fun id -> [ Inert(Micro_var(id)); Command(LOOKUP); ])
          args
      in
      [
        Inert(Micro_var(x0));
        Command(LOOKUP);
        Inert(Micro_var("*value"));
        Command(RETRIEVE);
        Command(GET);
      ] @
      arg_lookups @
      [ Command(CONVERT(List.length args)); ]

    | Assign(_, {body = Binop _; _}) (*(left op right)*) ->
      raise @@ Not_yet_implemented "Binops NYI"

    (* Raise statement *)
    | Raise (x) ->
      [
        Inert(Micro_var(x));
        Command(LOOKUP);
        Command(RAISE);
      ]

    (* Pass statement *)
    | Pass ->
      [
        Command(ADVANCE);
      ]

    (* Return statement *)
    | Return (x) ->
      let caller = Program_stack.top stack_body in
      let active_stmt =
        Body.get_stmt ctx.program @@ Stack_frame.get_active_uid caller
      in
      let x2 =
        match active_stmt with
        | Some({body = Assign(id, _); _}) -> id
        | _ -> failwith "Did not see assign on return from call!"
      in
      [
        Inert(Micro_var(x));
        Command(LOOKUP);
        Command(POP);
        Inert(Micro_var(x2));
        Command(BIND);
        Command(ADVANCE);
      ]

    (* Goto statement *)
    | Goto (uid) ->
      [
        Command(GOTO(uid));
      ]

    (* Goto statement *)
    | GotoIfNot (x, uid) ->
      [
        Inert(Micro_var(x));
        Command(LOOKUP);
        Command(GOTOIFNOT(uid));
      ]

    (* Catch statement *)
    | Catch _ -> failwith "Encountered catch with no raised value!"

    | Print _ ->  raise @@ Not_yet_implemented "Print statements NYI"
  in
  { prog with micro = Micro_instruction_stack.create new_micro_list }
;;

let rec step_program (prog : program_state) (ctx : program_context)
  : program_state =
  if Program_stack.is_empty prog.stack then
    prog
  else
    let next_prog =
      if Micro_instruction_stack.is_empty prog.micro
      then
        execute_stmt prog ctx
      else
        execute_micro_command prog ctx
    in
    step_program next_prog ctx
;;

let interpret_program (prog : modl) =
  (* TODO: Actual initialization *)
  let Module(stmts, _) = prog in
  let starting_ctx = { program = Body.create stmts; } in
  let global_memloc = Memloc(0) in
  let starting_frame =
    Stack_frame.create global_memloc @@ Body.get_first_uid starting_ctx.program
  in
  let starting_stack = Program_stack.singleton starting_frame in
  let starting_bindings = Bindings.empty in
  let starting_heap =
    Heap.singleton global_memloc @@ Bindings(starting_bindings) in
  let starting_micro = Micro_instruction_stack.empty in
  let starting_program =
    {
      micro = starting_micro;
      stack = starting_stack;
      heap = starting_heap;
    }
  in
  step_program starting_program starting_ctx
;;
