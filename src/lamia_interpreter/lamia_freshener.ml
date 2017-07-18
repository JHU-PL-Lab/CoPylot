open Batteries;;
open Jhupllib;;

open Lamia_evaluation_ast;;

let add_to_log = Logger_utils.make_logger "Lamia freshening";;
Logger_utils.set_default_logging_level `warn;;

module Value_variable_map = Map.Make(Value_variable);;
module Memory_variable_map = Map.Make(Memory_variable);;

module FresheningMonad :
sig
  type 'a m
  val return : 'a -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m

  val get_value_variable_freshening : value_variable -> value_variable m
  val get_memory_variable_freshening : memory_variable -> memory_variable m

  val make_value_variable_freshening : value_variable -> value_variable m
  val make_memory_variable_freshening : memory_variable -> memory_variable m

  val isolate_variable_freshening_scope : 'a m -> 'a m

  val sequence : 'a m list -> 'a list m

  val run : 'a m -> 'a
end =
struct
  type persistent_freshening_state =
    { next_freshening_index : int;
    };;
  type scoped_freshening_state =
    { bound_value_variables : value_variable Value_variable_map.t;
      bound_memory_variables : memory_variable Memory_variable_map.t;
    }
  type 'a m =
    persistent_freshening_state -> scoped_freshening_state ->
    'a * persistent_freshening_state * scoped_freshening_state
  ;;

  let fresh_str (pfx : string) : string m =
    fun pstate sstate ->
      let index = pstate.next_freshening_index in
      let pstate' = { next_freshening_index = index + 1 } in
      (pfx ^ "$$" ^ string_of_int index, pstate', sstate)
  ;;

  let return (x : 'a) : 'a m = fun pstate sstate -> (x, pstate, sstate);;
  let bind (x : 'a m) (f : 'a -> 'b m) : 'b m =
    fun pstate sstate ->
      let (v,pstate',sstate') = x pstate sstate in
      f v pstate' sstate'
  ;;

  let get_value_variable_freshening (x : value_variable) : value_variable m =
    fun pstate sstate ->
      match Value_variable_map.Exceptionless.find
              x sstate.bound_value_variables with
      | None -> (x, pstate, sstate)
      | Some x' -> (x', pstate, sstate)
  ;;

  let get_memory_variable_freshening (y : memory_variable) : memory_variable m =
    fun pstate sstate ->
      match Memory_variable_map.Exceptionless.find
              y sstate.bound_memory_variables with
      | None -> (y, pstate, sstate)
      | Some y' -> (y', pstate, sstate)
  ;;

  let make_value_variable_freshening
      ((Lamia_ast.Value_variable str) as x : value_variable)
    : value_variable m =
    fun pstate sstate ->
      let (str',pstate',sstate') = fresh_str str pstate sstate in
      let x' = Lamia_ast.Value_variable str' in
      let sstate'' =
        { sstate' with
          bound_value_variables =
            Value_variable_map.add x x' sstate'.bound_value_variables
        }
      in
      (x',pstate',sstate'')
  ;;

  let make_memory_variable_freshening
      ((Lamia_ast.Memory_variable str) as y : memory_variable)
    : memory_variable m =
    fun pstate sstate ->
      let (str',pstate',sstate') = fresh_str str pstate sstate in
      let y' = Lamia_ast.Memory_variable str' in
      let sstate'' =
        { sstate' with
          bound_memory_variables =
            Memory_variable_map.add y y' sstate'.bound_memory_variables
        }
      in
      (y',pstate',sstate'')
  ;;

  let isolate_variable_freshening_scope (x : 'a m) : 'a m =
    fun pstate sstate ->
      let (value, pstate', _) = x pstate sstate in
      (value, pstate', sstate)
  ;;

  let sequence (xs : 'a m list) : 'a list m =
    let rec loop (acc : 'a list) (xs' : 'a m list) : 'a list m =
      match xs' with
      | [] -> return acc
      | x::xs'' ->
        let%bind z = x in
        loop (z::acc) xs''
    in
    let%bind zs = loop [] xs in
    return @@ List.rev zs
  ;;

  let run (m : 'a m) : 'a =
    let initial_scoped_state =
      { bound_value_variables = Value_variable_map.empty;
        bound_memory_variables = Memory_variable_map.empty;
      }
    in
    let initial_persistent_state =
      { next_freshening_index = 1; }
    in
    let (value, _, _) = m initial_persistent_state initial_scoped_state in
    value
  ;;

end;;
open FresheningMonad;;


(* Note: ordering is important in this function.  The LHS is handled second so
   that free variables can be conditionally freshened before binding the LHS
   variables and freshening them. *)
let rec freshen_value_expression (e : value_expression) : value_expression m =
  match e with
  | Integer_literal n -> return @@ Integer_literal n
  | String_literal s -> return @@ String_literal s
  | Boolean_literal b -> return @@ Boolean_literal b
  | List_value ys ->
    let%bind ys' = sequence @@ List.map get_memory_variable_freshening ys in
    return @@ List_value ys'
  | Function_expression(xs, b) ->
    let%bind xs',b' =
      isolate_variable_freshening_scope @@
      let%bind xs'' = sequence @@ List.map make_value_variable_freshening xs in
      let%bind b'' = freshen_block b in
      return (xs'',b'')
    in
    return @@ Function_expression(xs',b')
  | None_literal -> return @@ None_literal
  | Empty_binding -> return @@ Empty_binding

and freshen_directive (directive : directive) : directive m =
  match directive with
  | Let_expression(x,e) ->
    let%bind e' = freshen_value_expression e in
    let%bind x' = make_value_variable_freshening x in
    return @@ Let_expression(x',e')
  | Let_alloc y ->
    let%bind y' = make_memory_variable_freshening y in
    return @@ Let_alloc y'
  | Let_alias_value(x1,x2) ->
    let%bind x2' = get_value_variable_freshening x2 in
    let%bind x1' = make_value_variable_freshening x1 in
    return @@ Let_alias_value(x1',x2')
  | Let_alias_memory(y1,y2) ->
    let%bind y2' = get_memory_variable_freshening y2 in
    let%bind y1' = make_memory_variable_freshening y1 in
    return @@ Let_alias_memory(y1',y2')
  | Let_binding_update(xlet,xobj,xkey,yval) ->
    let%bind xobj' = get_value_variable_freshening xobj in
    let%bind xkey' = get_value_variable_freshening xkey in
    let%bind yval' = get_memory_variable_freshening yval in
    let%bind xlet' = make_value_variable_freshening xlet in
    return @@ Let_binding_update(xlet',xobj',xkey',yval')
  | Let_binding_access(ylet,xobj,xkey) ->
    let%bind xobj' = get_value_variable_freshening xobj in
    let%bind xkey' = get_value_variable_freshening xkey in
    let%bind ylet' = make_memory_variable_freshening ylet in
    return @@ Let_binding_access(ylet',xobj',xkey')
  | Let_list_access(ylet,xlist,xindex) ->
    let%bind xlist' = get_value_variable_freshening xlist in
    let%bind xindex' = get_value_variable_freshening xindex in
    let%bind ylet' = make_memory_variable_freshening ylet in
    return @@ Let_list_access(ylet',xlist',xindex')
  | Let_list_slice(xlet,xlist,xindex1,xindex2) ->
    let%bind xlist' = get_value_variable_freshening xlist in
    let%bind xindex1' = get_value_variable_freshening xindex1 in
    let%bind xindex2' = get_value_variable_freshening xindex2 in
    let%bind xlet' = make_value_variable_freshening xlet in
    return @@ Let_list_slice(xlet',xlist',xindex1',xindex2')
  | Let_call_function(ylet,xfun,xargs) ->
    let%bind xfun' = get_value_variable_freshening xfun in
    let%bind xargs' =
      sequence @@ List.map get_value_variable_freshening xargs
    in
    let%bind ylet' = make_memory_variable_freshening ylet in
    return @@ Let_call_function(ylet',xfun',xargs')
  | Store(y,x) ->
    let%bind x' = get_value_variable_freshening x in
    let%bind y' = get_memory_variable_freshening y in
    return @@ Store(y',x')
  | Let_get(x,y) ->
    let%bind y' = get_memory_variable_freshening y in
    let%bind x' = make_value_variable_freshening x in
    return @@ Let_get(x',y')
  | Let_is(x,y1,y2) ->
    let%bind y2' = get_memory_variable_freshening y2 in
    let%bind y1' = get_memory_variable_freshening y1 in
    let%bind x' = make_value_variable_freshening x in
    return @@ Let_is(x',y1',y2')
  | Let_unop(x1,unop,x2) ->
    let%bind x2' = get_value_variable_freshening x2 in
    let%bind x1' = make_value_variable_freshening x1 in
    return @@ Let_unop(x1',unop,x2')
  | Let_binop(x1,x2,binop,x3) ->
    let%bind x2' = get_value_variable_freshening x2 in
    let%bind x3' = get_value_variable_freshening x3 in
    let%bind x1' = make_value_variable_freshening x1 in
    return @@ Let_binop(x1',x2',binop,x3')
  | Return y ->
    let%bind y' = get_memory_variable_freshening y in
    return @@ Return y'
  | If_result_value x ->
    let%bind x' = get_value_variable_freshening x in
    return @@ If_result_value x'
  | If_result_memory y ->
    let%bind y' = get_memory_variable_freshening y in
    return @@ If_result_memory y'
  | Raise y ->
    let%bind y' = get_memory_variable_freshening y in
    return @@ Raise y'
  | Try_except(b1,y,b2) ->
    let%bind b1' = freshen_block b1 in
    let%bind y',b2' =
      isolate_variable_freshening_scope @@
      let%bind y'' = make_memory_variable_freshening y in
      let%bind b2'' = freshen_block b2 in
      return @@ (y'',b2'')
    in
    return @@ Try_except(b1',y',b2')
  | Let_conditional_value(xlet,xcond,b1,b2) ->
    let%bind xcond' = get_value_variable_freshening xcond in
    let%bind b1' = isolate_variable_freshening_scope @@ freshen_block b1 in
    let%bind b2' = isolate_variable_freshening_scope @@ freshen_block b2 in
    let%bind xlet' = make_value_variable_freshening xlet in
    return @@ Let_conditional_value(xlet',xcond',b1',b2')
  | Let_conditional_memory(ylet,xcond,b1,b2) ->
    let%bind xcond' = get_value_variable_freshening xcond in
    let%bind b1' = isolate_variable_freshening_scope @@ freshen_block b1 in
    let%bind b2' = isolate_variable_freshening_scope @@ freshen_block b2 in
    let%bind ylet' = make_memory_variable_freshening ylet in
    return @@ Let_conditional_memory(ylet',xcond',b1',b2')
  | While(ycond,block) ->
    let%bind ycond' = get_memory_variable_freshening ycond in
    let%bind block' =
      isolate_variable_freshening_scope @@ freshen_block block
    in
    return @@ While(ycond',block')

and freshen_statement (statement : statement) : statement m =
  match statement with
  | Statement(d) ->
    let%bind d' = freshen_directive d in
    return @@ Statement d'
  | _ ->
    raise @@ Utils.Invariant_failure "Attempted to freshen running code"

and freshen_statement_list (ss : statement list) : statement list m =
  sequence @@ List.map freshen_statement ss

and freshen_block (block : block) : block m =
  let Block(ss) = block in
  let%bind ss' = freshen_statement_list ss in
  return @@ Block(ss')
;;

let freshen_block_top (block : block) : block =
  run @@ freshen_block block
;;
