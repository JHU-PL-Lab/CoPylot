open OUnit2;;
open Batteries;;
open Jhupllib;;

open Lamia_parser;;

open Analysis_types;;

Logger_utils.set_default_logging_level `warn;;

module Value_ord =
struct
  type t = value
  [@@deriving eq, ord, show]
end
;;

module Answer_set = Set.Make(Value_ord);;

let make_query analysis target_str =
  let open Analysis_lookup in
  let open Analysis_grammar in
  let results, _ =
    match String.get target_str 0 with
    | '&' ->
      lookup_memory Program_state.End (Memory_variable(target_str)) analysis
    | _ ->
      lookup_value Program_state.End (Value_variable(target_str)) analysis
  in
  Answer_set.of_enum results
;;

let gen_lamia_test
    (name : string)
    (prog : string)
    (target : string)
    (expected : value list) =
  name >::
  (fun _ ->
     let parsed_prog = parse_from_string prog in
     let lifted = fst @@ Analysis_lift_ast.lift_block_top parsed_prog in
     let analysis = fst @@ Analysis_construct_cfg.construct_analysis lifted in
     let actual = make_query analysis target in
     let expected = Answer_set.of_list expected in
     assert_equal
       ~cmp:Answer_set.equal
       ~printer:(Pp_utils.pp_to_string (Pp_utils.pp_set pp_value Answer_set.enum))
       expected
       actual
  )
;;

let literal_tests =
  [
    gen_lamia_test "int_pos_test" "let x = 5;;" "x" [Integer_value (Int_exact 5)];
    gen_lamia_test "int_neg_test" "let x = -5;;" "x" [Integer_value (Int_exact (-5))];
    gen_lamia_test "int_zero_test" "let x = 0;;" "x" [Integer_value (Int_exact 0)];
    gen_lamia_test "string_test" "let x = \"foo\";;" "x" [String_value (String_exact "foo")];
    gen_lamia_test "bool_true_test" "let x = True;;" "x" [Boolean_value true];
    gen_lamia_test "bool_false_test" "let x = False;;" "x" [Boolean_value false];
    gen_lamia_test "empty_binding_test" "let x = {};;" "x" [Object_value AbstractStringMap.empty];
    gen_lamia_test "none_test" "let x = None;;" "x" [None_value];

    gen_lamia_test "list_test_empty" "let x = [];;" "x" [List_value(List_exact ([],0))];
    gen_lamia_test "list_test_singleton" "let x = 4; let &y = alloc; store &y x; let z = [&y,&y];;" "z" [(List_value(List_exact ([Memloc(Statement (-2,(Let_alloc (Memory_variable "&y"))));(Memloc(Statement (-2,(Let_alloc (Memory_variable "&y")))))],2)))];
    gen_lamia_test "list_test_mixed" "let x1 = 4; let &y1 = alloc; let x2 = 0; let &y2 = alloc; let x3 = -5; let &y3 = alloc; store &y1 x1; store &y2 x2; store &y3 x3; let z = [&y1,&y2,&y3];;" "z" [(List_value(List_exact ([Memloc(Statement (-2,(Let_alloc (Memory_variable "&y1"))));Memloc(Statement (-4,(Let_alloc (Memory_variable "&y2")))); Memloc(Statement (-6,(Let_alloc (Memory_variable "&y3"))))],3)))];
    gen_lamia_test "list_test_mixed_type" "let x1 = 4; let &y1 = alloc; let x2 = false; let &y2 = alloc; let x3 = \"foo\"; let &y3 = alloc; store &y1 x1; store &y2 x2; store &y3 x3; let z = [&y1,&y2,&y3];;" "z" [(List_value(List_exact ([Memloc(Statement (-2,(Let_alloc (Memory_variable "&y1"))));Memloc(Statement (-4,(Let_alloc (Memory_variable "&y2")))); Memloc(Statement (-6,(Let_alloc (Memory_variable "&y3"))))],3)))];

    gen_lamia_test "simple_func_test" "let f = def () {let &y = alloc; return &y};;" "f"
      [Function_value ([], Block [Statement (-1, Let_alloc (Memory_variable "&y")); Statement (-2, Return (Memory_variable "&y"));])];
    gen_lamia_test "simple_func_test2" "let f = def (x){let &y = alloc; store &y x; return &y;};;" "f" [Function_value ([Value_variable "x"],Block[Statement (-1,Let_alloc (Memory_variable "&y"));Statement (-2, Store (Memory_variable "&y", Value_variable "x")); Statement (-3, (Return (Memory_variable "&y")))])];
  ]
;;

let skip_tests =
  [
    gen_lamia_test "basic_skip_test" "let x = 2; let y = -2;;" "x" [Integer_value (Int_exact 2)];
    gen_lamia_test "don't_skip_test" "let x = 2; let x = -2;;" "x" [Integer_value (Int_exact (-2))];

    gen_lamia_test "skip_operator_test" "let x = 1; let y = -1; let z = x int+ y;;" "x" [Integer_value (Int_exact 1)];
    gen_lamia_test "don't_skip_operator_test" "let x = 1; let y = -1; let x = y int- x;;" "x" [Integer_value (Int_lossy Neg)];

    gen_lamia_test "skip_if_test" "let x = 1; let y = True; let z = if y then {ifresult x;} else {ifresult y;};;" "x" [Integer_value (Int_exact 1)];
    gen_lamia_test "don't_skip_if_test" "let x = 1; let y = False; let x = if y then {ifresult x;} else {ifresult y;};;" "x" [Boolean_value false];

    gen_lamia_test "skip_while_test" "let x = True; let &y = alloc; store &y x; @2:while &y {let x = False; let x1 = 3; @3:store &y x;};@1:let x2 = get &y;;" "x" [Boolean_value true];
    gen_lamia_test "skip_while_test_2" "let x = True; let &y = alloc; store &y x; @2:while &y {let x = False; let x1 = 3; @3:store &y x;};@1:let x2 = get &y;;" "x1" [];

    gen_lamia_test "skip_try_test" "let x = 1; try {let x = -1;} except &y {let x = 0;};;" "x" [Integer_value (Int_exact 1)];

    gen_lamia_test "skip_funcdef_test" "let x = 1; let f = def () {let &y = alloc; return &y};;" "x" [Integer_value (Int_exact 1)];
    gen_lamia_test "skip_call_test" "let x = 1; let f = def () {let &y = alloc; return &y}; let &z = f();;" "x" [Integer_value (Int_exact 1)];

  ]
;;

let operator_tests =
  [
    gen_lamia_test "not_test_x" "let x = True; let y = not x;;" "x" [Boolean_value true];
    gen_lamia_test "not_test_y" "let x = True; let y = not x;;" "y" [Boolean_value false];
    gen_lamia_test "not_test_overwrite" "let x = True; let x = not x;;" "x" [Boolean_value false];

    gen_lamia_test "isint_test_true" "let x = -7; let y = isint x;;" "y" [Boolean_value true];
    gen_lamia_test "isint_test_false" "let x = \"\"; let y = isint x;;" "y" [Boolean_value false];

    gen_lamia_test "int_add_exact_pos" "let x = 1; let y = 2; let z = x int+ y;;" "z" [Integer_value (Int_lossy Pos)];
    (* gen_lamia_test "int_add_test2" "let x = 1; let y = 0; let z = x int+ y" "z" [Integer_value (Int_lossy Pos)]; *)
    gen_lamia_test "int_add_exact_zero" "let x = 1; let y = -1; let z = x int+ y;;" "z" [Integer_value (Int_exact 0)];
    gen_lamia_test "int_add_exact_neg" "let x = 1; let y = -2; let z = x int+ y;;" "z" [Integer_value (Int_lossy Neg)];
    gen_lamia_test "int_add_lossy_pos" "let x = 1; let y = 2; let z = x int+ x; let w = y int+ y; let v = z int+ w;;" "v" [Integer_value (Int_lossy Pos);];
    gen_lamia_test "int_add_lossy_neg" "let x = -1; let y = -2; let z = x int+ x; let w = y int+ y; let v = z int+ w;;" "v" [Integer_value (Int_lossy Neg);];
    gen_lamia_test "int_add_lossy_unknown" "let x = 1; let y = -2; let z = x int+ x; let w = y int+ y; let v = z int+ w;;" "v" [Integer_value (Int_lossy Pos);Integer_value (Int_exact 0);Integer_value (Int_lossy Neg);];
    gen_lamia_test "int_add_mixed_pos" "let x = 1; let y = 2; let z = x int+ x;let v = z int+ y;;" "v" [Integer_value (Int_lossy Pos);];
    gen_lamia_test "int_add_mixed_neg" "let x = -1; let y = -2; let z = x int+ x; let v = z int+ y;;" "v" [Integer_value (Int_lossy Neg);];
    gen_lamia_test "int_add_mixed_unknown1" "let x = 1; let y = -1; let z = x int+ x; let v = z int+ y;;" "v" [Integer_value (Int_lossy Pos);Integer_value (Int_exact 0);Integer_value (Int_lossy Neg);];
    gen_lamia_test "int_add_mixed_unknown2" "let x = 1; let y = -1; let z = y int+ y; let v = z int+ x;;" "v" [Integer_value (Int_lossy Pos);Integer_value (Int_exact 0);Integer_value (Int_lossy Neg);];

    gen_lamia_test "list_index_empty" "let x = []; let y = 0; let &w = x[y];;" "&w" [];
    gen_lamia_test "list_index_singleton_exact_miss" "let x = 4; let &y = alloc; store &y x; let z = [&y]; let &w = z[x];;" "&w" [];
    gen_lamia_test "list_index_singleton_exact_hit" "let x = 0; let &y = alloc; store &y x; let z = [&y]; let &w = z[x];;" "&w" [Integer_value (Int_exact 0)];
    gen_lamia_test "list_index_singleton_lossy" "let x = 1; let v = x int+ x; let &y = alloc; store &y x; let z = [&y]; let &w = z[v];;" "&w" [Integer_value (Int_exact 1)];
    gen_lamia_test "list_index_mixed_exact" "let x1 = 1; let &y1 = alloc; let x2 = 0; let &y2 = alloc; store &y1 x1; store &y2 x2; let z = [&y1,&y2]; let &w = z[x1];;" "&w" [Integer_value (Int_exact 0)];
    gen_lamia_test "list_index_head" "let x1 = 4; let &y1 = alloc; let x2 = 0; let &y2 = alloc; store &y1 x1; store &y2 x2; let z = [&y1,&y2]; let &w = z[x2];;" "&w" [Integer_value (Int_exact 4)];
    gen_lamia_test "list_index_segfault" "let x1 = 4; let &y1 = alloc; let x2 = -1; let &y2 = alloc; store &y1 x1; store &y2 x2; let z = [&y1,&y2]; let &w = z[x2];;" "&w" [];
    gen_lamia_test "list_index_mixed_type_lossy" "let x1 = 4; let &y1 = alloc; let x2 = False; let &y2 = alloc; let x3 = \"foo\"; let &y3 = alloc; store &y1 x1; store &y2 x2; store &y3 x3; let z = [&y1,&y2,&y3]; let v = x1 int+ x1; let &w = z[v];;" "&w" [Integer_value (Int_exact 4); Boolean_value false; String_value (String_exact "foo")];

    gen_lamia_test "list_slice_empty" "let x = []; let y = 0; let w = x[y:y];;" "w" [];
    gen_lamia_test "list_slice_singleton" "let x = 1; let &y = alloc; store &y x; let z = [&y]; let n = 0; let w = z[n:x];;" "w" [(List_value(List_lossy [Memloc(Statement (-2,(Let_alloc (Memory_variable "&y"))))]))];
    gen_lamia_test "list_slice_segfault" "let x1 = -1; let &y1 = alloc; let x2 = False; let &y2 = alloc; store &y1 x1; store &y2 x2; let z = [&y1,&y2]; let n = 4; let w = z[x1:n];;" "w" [];
    gen_lamia_test "list_slice_zero_pos" "let x1 = 0; let &y1 = alloc; let x2 = False; let &y2 = alloc; store &y1 x1; store &y2 x2; let z = [&y1,&y2]; let n = 4; let w = z[x1:n];;" "w" [(List_value(List_lossy [Memloc(Statement (-2,(Let_alloc (Memory_variable "&y1"))));Memloc(Statement (-4,(Let_alloc (Memory_variable "&y2"))))]))];
    gen_lamia_test "list_slice_zero_zero" "let x1 = 0; let &y1 = alloc; let x2 = False; let &y2 = alloc; store &y1 x1; store &y2 x2; let z = [&y1,&y2]; let n = 0; let w = z[x1:n];;" "w" [(List_value(List_lossy [Memloc(Statement (-2,(Let_alloc (Memory_variable "&y1"))))]))];
    gen_lamia_test "list_slice_pos_pos" "let x1 = 1; let &y1 = alloc; let x2 = False; let &y2 = alloc; store &y1 x1; store &y2 x2; let z = [&y1,&y2]; let n = 4; let w = z[x1:n];;" "w" [(List_value(List_lossy [Memloc(Statement (-4,(Let_alloc (Memory_variable "&y2"))))]))];

    gen_lamia_test "list_concat_empty" "let x = []; let y = []; let z = x || y;;" "z" [List_value (List_exact ([], 0))];
    gen_lamia_test "list_concat_single" "let &l = alloc; let x = []; let n = 4; store &l n; let y = [&l]; let z = x || y;;" "z" [List_value (List_exact ( [Memloc (Statement (-1, Let_alloc (Memory_variable "&l")))], 1))];
  ]
;;

let store_tests =
  [
    gen_lamia_test "store_test" "let x = 4;let &y = alloc; store &y x;;" "&y" [Integer_value (Int_lossy Pos)];
    gen_lamia_test "store_fun_test" "let f = def (x) {let &y = alloc; store &y x; return &y}; let &y = alloc; store &y f;;" "&y" [Function_value ([Value_variable "x"],
    Block [Statement (-1, Let_alloc (Memory_variable "&y"));
           Statement (-2, Store ((Memory_variable "&y"),(Value_variable "x")));
           Statement (-3, Return (Memory_variable "&y"));])];
    gen_lamia_test "get_test" "let x = 4; let &y = alloc; store &y x; let z = get &y;;" "z" [Integer_value (Int_lossy Pos)];
    gen_lamia_test "store_rebind_test" "let x = 4;let &y = alloc; let x = \"foo\"; store &y x; let z = get &y;;" "z" [String_value (String_exact "foo")];
    gen_lamia_test "is_test_true" "let x = 4; let &y1 = alloc; store &y1 x; let &y2 = &y1; let z = &y1 is &y2;;" "z" [Boolean_value true];
    gen_lamia_test "is_test_false" "let &y1 = alloc; let &y2 = alloc; let z = &y1 is &y2;;" "z" [Boolean_value false];
    gen_lamia_test "is_test_false_2" "let x1 = 4; let x2 = 4; let &y1 = alloc; let &y2 = alloc; store &y1 x1; store &y2 x2; let z = &y1 is &y2;;" "z" [Boolean_value false];
    gen_lamia_test "is_test_false_3" "let x = 4; let &y1 = alloc; let &y2 = alloc; store &y1 x; store &y2 x; let z = &y1 is &y2;;" "z" [Boolean_value false];
  ]

let if_tests =
  [
    gen_lamia_test "if_true_x_test" "let x = True; let y = if x then {let z = 1; ifresult z;} else {let z = -1; ifresult z;};;" "y" [Integer_value (Int_lossy Pos)];
    gen_lamia_test "if_false_x_test" "let x = False; let y = if x then {let z = 1; ifresult z; } else {let z = -1; ifresult z;};;" "y" [Integer_value (Int_lossy Neg)];

    gen_lamia_test "if_true_y_test" "let x = True; let &y = if x then {let z = 1; let &w = alloc; store &w z; ifresult &w;} else {let z = -1; let &w = alloc; store &w z; ifresult &w;};;" "&y" [Integer_value (Int_lossy Pos)];
    gen_lamia_test "if_false_y_test" "let x = False; let &y = if x then {let z = 1; let &w = alloc; store &w z; ifresult &w;} else {let z = -1; let &w = alloc; store &w z; ifresult &w;};;" "&y" [Integer_value (Int_lossy Neg)];
    gen_lamia_test "if_parent_x_test" "let x = True; let y = if x then {ifresult x;} else {ifresult x;};;" "y" [Boolean_value true];
    gen_lamia_test "if_else_parent_x_test" "let x = False; let y = if x then {ifresult x;} else {ifresult x;};;" "y" [Boolean_value false];
  ]
;;

let while_tests =
  [
    gen_lamia_test "while_result_test" "let x = True; let &y = alloc; store &y x; @2:while &y {let x = False; let x1 = 3; @3:store &y x;};@1:let x2 = get &y;;" "x2" [Boolean_value true; Boolean_value false;];
    gen_lamia_test "while_scope_test1" "let x = True; let &y = alloc; store &y x; let &z = alloc; @2:while &y {let x = False; let x1 = 3; store &z x1; @3:store &y x;};@1:let x2 = get &y;;" "&z" [Integer_value (Int_lossy Pos)];
    gen_lamia_test "while_scope_test2" "let x = True; let &y = alloc; store &y x; let &z = alloc; let x1 = 3; @2:while &y {let x = False; store &z x1; @3:store &y x;};@1:let x2 = get &y;;" "&z" [Integer_value (Int_lossy Pos)];
  ]
;;

let function_call_tests =
  [
    gen_lamia_test "simple_call_test" "let f = def () {let &y = alloc; let x = 1; store &y x; return &y}; let &z = f();;" "&z" [Integer_value (Int_lossy Pos)];
    gen_lamia_test "free_var_test1" "let x = 1; let &y = alloc; store &y x; let f = def () {return &y}; let &z = f();;" "&z" [Integer_value (Int_lossy Pos)];
    gen_lamia_test "free_var_test2" "let x = 1; let f = def () {let &y = alloc; store &y x; return &y}; let &z = f();;" "&z" [Integer_value (Int_lossy Pos)];
    gen_lamia_test "free_var_test3" "let x = 1; let f = def () {let &y = alloc; store &y x; return &y}; let x = -1; let &z = f();;" "&z" [Integer_value (Int_lossy Pos)];
    gen_lamia_test "arg_list_test" "let x = 1; let y = True; let f = def (m,n) {let &y = alloc; store &y n; return &y}; let &z = f(x,y);;" "&z" [Boolean_value true];
    gen_lamia_test "call_within_call_test" "let f = def () {let &y = alloc; let x = 1; store &y x; return &y}; let g = def () {let &y = f(); return &y}; let &z = g();;" "&z" [Integer_value (Int_lossy Pos)];
    gen_lamia_test "return_in_if_test" "let f = def (x) {let &y = if x then {let u = 4; let &y = alloc; store &y u; return &y} else {let &y = alloc; store &y x; return &y};}; let x = True; let &z = f(x);;" "&z" [Integer_value (Int_lossy Pos)];
    gen_lamia_test "return_self_test" "let &y = alloc; let f = def () {return &y;}; store &y f; let &z = f();;" "&z" [Function_value ([], Block [Statement (-2, Return (Memory_variable "&y"))])];

    gen_lamia_test "recursive_call_test" "let &fun = alloc; let f = def (x) {let &y = if x then {let &y = alloc; let x2 = not x; let f = get &fun; let &y = f(x2); return &y} else {let &y = alloc; store &y x; return &y};}; store &fun f; let x = True; let &z = f(x);;" "&z" [Boolean_value true; Boolean_value false];
    gen_lamia_test "condition_test" "let &fun = alloc; let f = def (x) {let &y = if x then {let &y = alloc; let x = False; store &y x; return &y} else {let &y = alloc; let x = True; store &y x; return &y};}; store &fun f; let x = True; let &z = f(x);;" "&z" [Boolean_value false];
  ]
;;

let try_tests =
  [
    gen_lamia_test "basic_try_test" "let x = 0; let &y = alloc; store &y x; try {let x = 1; store &y x;} except &z {let x = -1; store &y x;};;" "&y" [Integer_value (Int_lossy Pos)];
    gen_lamia_test "basic_raise_test" "let x = 0; let &y = alloc; store &y x; try {let x = 1; store &y x; raise &y; } except &z {let x = -1; store &y x;};;" "&y" [Integer_value (Int_lossy Neg)];
    gen_lamia_test "raise_value_test" "let x = 0; let &y = alloc; store &y x; try {let x = 1; store &y x; raise &y; } except &z {let x = -1; store &y x;};;" "&z" [];
  ]
;;

let tests =
  "test_lamia_parser" >:::
  literal_tests @
  skip_tests @
  operator_tests @
  store_tests @
  if_tests @
  while_tests @
  function_call_tests @
  try_tests @
  [

  ]
;;
