open OUnit2;;
open Batteries;;
open Jhupllib;;
open Pp_utils;;

open Python2_ast_types;;
open Python2_abstract_ast;;
open Python2_abstract_ast_pretty;;
open Python2_ast_pipeline;;

let string_of_modl m = Pp_utils.pp_to_string pp_modl m;;

let parse_to_abstract_safe prog short_names =
  try
    parse_to_abstract prog short_names
  with
  | Python2_parser.Parse_error p ->
    let open Lexing in
    assert_failure (Printf.sprintf "Error in line %d, col %d."
                      p.pos_lnum p.pos_cnum)
;;

(* Return true if there are no duplicates (which is what we want) *)
let rec check_for_duplicates (lst : int list) : bool =
  let sorted = List.sort compare lst in
  match sorted with
  | [] -> true
  | head::rest ->
    let _, value =
      List.fold_left
        (fun (prev : int * bool) (next : int) ->
           next, (snd prev) && (next != fst prev))
        (head, true)
        rest
    in value

let rec verify_unique_uids = function
  | Module(body, uid) ->
    let uids = List.concat (List.map collect_uids_stmt body) in
    check_for_duplicates (uid::uids)

and collect_uids_stmt s =
  let rest =
    match s.body with
    | Assign (_, {uid = u; body = Literal(FunctionVal(_, body)); _})
      -> [u] @ List.concat (List.map collect_uids_stmt body)
    | Assign (_, {uid = u; _})
      -> [u]
    | _ -> []
  in
  s.uid::rest
;;

let get_call_def =
  "@ 137:    :F:  $norm51 = def ($norm0) {" ^
  "\n@   3:   1:T:    pass;" ^
  "\n@   6:   1:T:    $norm2 = *type;" ^
  "\n@   8:   1:T:    $norm3 = $norm2($norm0);" ^
  "\n@  10:   1:T:    $norm4 = *method_wrapper_type;" ^
  "\n@  12:   1:T:    $norm5 = $norm3 is $norm4;" ^
  "\n@  14:   1:T:    $norm8 = *bool;" ^
  "\n@  16:   1:T:    $norm9 = $norm8($norm5);" ^
  "\n@  28:   1:T:    goto 27 if not $norm9;" ^
  "\n@  18:   1:T:    $norm10 = false;" ^
  "\n@  20:   1:T:    $norm7 = $norm10;" ^
  "\n@  26:   1:T:    goto 25;" ^
  "\n@  27:   1:T:    pass;" ^
  "\n@  22:   1:T:    $norm11 = true;" ^
  "\n@  24:   1:T:    $norm7 = $norm11;" ^
  "\n@  25:   1:T:    pass;" ^
  "\n@  30:   1:T:    $norm6 = $norm7;" ^
  "\n@  32:   1:T:    $norm12 = *bool;" ^
  "\n@  34:   1:T:    $norm13 = $norm12($norm6);" ^
  "\n@ 108:   1:T:    goto 4 if not $norm13;" ^
  "\n@  38:  35:T:    $norm19 = $norm0.__getattribute__;" ^
  "\n@  40:  35:T:    $norm14 = $norm19;" ^
  "\n@  42:  35:T:    $norm20 = \"__call__\";" ^
  "\n@  44:  35:T:    $norm22 = *get_call($norm14);" ^
  "\n@  46:  35:T:    $norm21 = $norm22;" ^
  "\n@  48:  35:T:    $norm23 = $norm21($norm20);" ^
  "\n@  50:  35:T:    $norm17 = $norm23;" ^
  "\n@ 105:   1:T:    goto 36;" ^
  "\n@  35:   1:T:    catch $norm18;" ^
  "\n@  52:   1:T:    $norm24 = *type;" ^
  "\n@  54:   1:T:    $norm25 = $norm24($norm18);" ^
  "\n@  56:   1:T:    $norm26 = *AttributeError;" ^
  "\n@  58:   1:T:    $norm27 = $norm25 is $norm26;" ^
  "\n@  60:   1:T:    $norm28 = *bool;" ^
  "\n@  62:   1:T:    $norm29 = $norm28($norm27);" ^
  "\n@ 104:   1:T:    goto 103 if not $norm29;" ^
  "\n@  64:   1:T:    $norm16 = $norm18;" ^
  "\n@  68:  65:T:    $norm31 = $norm0.__getattr__;" ^
  "\n@  70:  65:T:    $norm15 = $norm31;" ^
  "\n@  89:   1:T:    goto 66;" ^
  "\n@  65:   1:T:    catch $norm30;" ^
  "\n@  72:   1:T:    $norm32 = *type;" ^
  "\n@  74:   1:T:    $norm33 = $norm32($norm30);" ^
  "\n@  76:   1:T:    $norm34 = *AttributeError;" ^
  "\n@  78:   1:T:    $norm35 = $norm33 is $norm34;" ^
  "\n@  80:   1:T:    $norm36 = *bool;" ^
  "\n@  82:   1:T:    $norm37 = $norm36($norm35);" ^
  "\n@  88:   1:T:    goto 87 if not $norm37;" ^
  "\n@  83:   1:T:    raise $norm16;" ^
  "\n@  86:   1:T:    goto 85;" ^
  "\n@  87:   1:T:    pass;" ^
  "\n@  84:   1:T:    raise $norm30;" ^
  "\n@  85:   1:T:    pass;" ^
  "\n@  66:   1:T:    pass;" ^
  "\n@  91:   1:T:    $norm38 = \"__call__\";" ^
  "\n@  93:   1:T:    $norm40 = *get_call($norm15);" ^
  "\n@  95:   1:T:    $norm39 = $norm40;" ^
  "\n@  97:   1:T:    $norm41 = $norm39($norm38);" ^
  "\n@  99:   1:T:    $norm17 = $norm41;" ^
  "\n@ 102:   1:T:    goto 101;" ^
  "\n@ 103:   1:T:    pass;" ^
  "\n@ 100:   1:T:    raise $norm18;" ^
  "\n@ 101:   1:T:    pass;" ^
  "\n@  36:   1:T:    pass;" ^
  "\n@ 107:   1:T:    $norm0 = $norm17;" ^
  "\n@ 109:   1:T:    goto 3;" ^
  "\n@   4:   1:T:    pass;" ^
  "\n@ 134:    :F:    goto 2;" ^
  "\n@   1:    :F:    catch $norm1;" ^
  "\n@ 111:    :F:    $norm42 = *type;" ^
  "\n@ 113:    :F:    $norm43 = $norm42($norm1);" ^
  "\n@ 115:    :F:    $norm44 = *AttributeError;" ^
  "\n@ 117:    :F:    $norm45 = $norm43 is $norm44;" ^
  "\n@ 119:    :F:    $norm46 = *bool;" ^
  "\n@ 121:    :F:    $norm47 = $norm46($norm45);" ^
  "\n@ 133:    :F:    goto 132 if not $norm47;" ^
  "\n@ 123:    :F:    $norm48 = *TypeError;" ^
  "\n@ 125:    :F:    $norm49 = \"Object is not callable\";" ^
  "\n@ 127:    :F:    $norm50 = $norm48($norm49);" ^
  "\n@ 128:    :F:    raise $norm50;" ^
  "\n@ 131:    :F:    goto 130;" ^
  "\n@ 132:    :F:    pass;" ^
  "\n@ 129:    :F:    raise $norm1;" ^
  "\n@ 130:    :F:    pass;" ^
  "\n@   2:    :F:    pass;" ^
  "\n@ 135:    :F:    return $norm0;" ^
  "\n};" ^
  "\n@ 139:    :F:  *get_call = $norm51;\n"
;;

let gen_module_test (name : string) (prog : string)
    (expected : string)=
  name>::
  ( fun _ ->
      let actual = parse_to_abstract_safe prog true in
      Python2_ast_simplifier.reset_unique_name ();
      Python2_ast_normalizer.reset_unique_name ();
      let distinct_uids = verify_unique_uids actual in
      assert_bool ("Repeated UIDs:\n" ^ string_of_modl actual)
        distinct_uids;
      let pyssembly = pp_to_string pp_modl actual in
      let full_expected = get_call_def ^ expected in
      assert_equal ~printer:(fun x -> x) ~cmp:String.equal full_expected pyssembly
  )
;;

let gen_literal_test name prog exp =
  gen_module_test name prog
    ("@ 141:    :F:  $norm52 = " ^ exp ^ ";")
;;

let literal_tests =
  [
    gen_literal_test "int+_test" "4" "Int+";
    gen_literal_test "int0_test" "0" "Int0";
    gen_literal_test "int-_test" "-4" "Int-";

    gen_literal_test "float+_test" "4.0" "Float+";
    gen_literal_test "float0_test" "0.0" "Float0";
    gen_literal_test "float-_test" "-4.0" "Float-";

    gen_literal_test "string_test" "'foo'" "\"foo\"";

    gen_literal_test "bool_true_test" "True" "true";
    gen_literal_test "bool_false_test" "False" "false";

    gen_literal_test "none_test" "None" "*None";
  ]
;;

(* let stmt_tests =
   [
    gen_module_test "return_test" "return x" "@   1:    :F:  return x;";

    gen_module_test "print_test1" "print x" "@   1:    :F:  print x;";
    gen_module_test "print_test2" "print x,y" "@   1:    :F:  print x, y;";

    gen_module_test "raise_test" "raise x" "@   1:    :F:  raise x;";

    gen_module_test "pass_test" "pass" "@   1:    :F:  pass;";
   ]
   ;;

   let attribute_test = gen_module_test "attribute_test"
    "x.mem"
    "@   2:    :F:  $norm0 = x.mem;"
   ;;

   let call_test = gen_module_test "call_test"
    "foo()"
    "@   2:    :F:  $norm0 = foo();"
   ;;

   let call_args_test = gen_module_test "call_args_test"
    "foo(1,x,2+3)"
    ("@   2:    :F:  $norm0 = Int+;" ^
     "\n@   4:    :F:  $norm1 = Int+;" ^
     "\n@   6:    :F:  $norm2 = $norm1.__add__;" ^
     "\n@   8:    :F:  $norm3 = Int+;" ^
     "\n@  10:    :F:  $norm4 = $norm2($norm3);" ^
     "\n@  12:    :F:  $norm5 = foo($norm0, x, $norm4);")

   ;;

   let assign_test = gen_module_test "assign_test"
    "x = 5"
    ("@   2:    :F:  $norm0 = Int+;" ^
     "\n@   4:    :F:  $simp0 = $norm0;" ^
     "\n@   6:    :F:  x = $simp0;")
   ;;

   let augassign_test = gen_module_test "augassign_test"
    "x += 5"
    begin
      "@   4:   1:F:  $norm1 = x.__iadd__;" ^
      "\n@   6:   1:F:  $simp4 = $norm1;" ^
      "\n@   8:   1:F:  $simp1 = $simp4;" ^
      "\n@  34:    :F:  goto 2;" ^
      "\n@   1:    :F:  catch $norm0;" ^
      "\n@  10:    :F:  $norm2 = *type;" ^
      "\n@  12:    :F:  $norm3 = $norm2($norm0);" ^
      "\n@  14:    :F:  $norm4 = *AttributeError;" ^
      "\n@  16:    :F:  $norm5 = $norm3.__eq__;" ^
      "\n@  18:    :F:  $norm6 = $norm5($norm4);" ^
      "\n@  20:    :F:  $norm7 = *bool;" ^
      "\n@  22:    :F:  $norm8 = $norm7($norm6);" ^
      "\n@  33:    :F:  goto 32 if not $norm8;" ^
      "\n@  24:    :F:  $norm9 = x.__add__;" ^
      "\n@  26:    :F:  $simp3 = $norm9;" ^
      "\n@  28:    :F:  $simp1 = $simp3;" ^
      "\n@  31:    :F:  goto 30;" ^
      "\n@  32:    :F:  pass;" ^
      "\n@  29:    :F:  raise $norm0;" ^
      "\n@  30:    :F:  pass;" ^
      "\n@   2:    :F:  pass;" ^
      "\n@  36:    :F:  $norm10 = Int+;" ^
      "\n@  38:    :F:  $norm11 = $simp1($norm10);" ^
      "\n@  40:    :F:  $simp2 = $norm11;" ^
      "\n@  42:    :F:  x = $simp2;"
    end
   ;;

   let augassign_to_member_test = gen_module_test "augassign_to_member_test"
    "obj.mem += 5"
    begin
      "@   2:    :F:  $simp0 = obj;" ^
      "\n@   6:   3:F:  $norm1 = $simp0.mem;" ^
      "\n@   8:   3:F:  $norm2 = $norm1.__iadd__;" ^
      "\n@  10:   3:F:  $simp4 = $norm2;" ^
      "\n@  12:   3:F:  $simp1 = $simp4;" ^
      "\n@  40:    :F:  goto 4;" ^
      "\n@   3:    :F:  catch $norm0;" ^
      "\n@  14:    :F:  $norm3 = *type;" ^
      "\n@  16:    :F:  $norm4 = $norm3($norm0);" ^
      "\n@  18:    :F:  $norm5 = *AttributeError;" ^
      "\n@  20:    :F:  $norm6 = $norm4.__eq__;" ^
      "\n@  22:    :F:  $norm7 = $norm6($norm5);" ^
      "\n@  24:    :F:  $norm8 = *bool;" ^
      "\n@  26:    :F:  $norm9 = $norm8($norm7);" ^
      "\n@  39:    :F:  goto 38 if not $norm9;" ^
      "\n@  28:    :F:  $norm10 = $simp0.mem;" ^
      "\n@  30:    :F:  $norm11 = $norm10.__add__;" ^
      "\n@  32:    :F:  $simp3 = $norm11;" ^
      "\n@  34:    :F:  $simp1 = $simp3;" ^
      "\n@  37:    :F:  goto 36;" ^
      "\n@  38:    :F:  pass;" ^
      "\n@  35:    :F:  raise $norm0;" ^
      "\n@  36:    :F:  pass;" ^
      "\n@   4:    :F:  pass;" ^
      "\n@  42:    :F:  $norm12 = Int+;" ^
      "\n@  44:    :F:  $norm13 = $simp1($norm12);" ^
      "\n@  46:    :F:  $simp2 = $norm13;" ^
      "\n@  48:    :F:  $norm14 = $simp0.__setattr__;" ^
      "\n@  50:    :F:  $norm15 = \"mem\";" ^
      "\n@  52:    :F:  $norm16 = $norm14($norm15, $simp2);"
    end
   ;;

   let augassign_to_list_test = gen_module_test "augassign_to_list_test"
    "lst[1:2] += 5"
    begin
      "@   2:    :F:  $simp0 = lst;" ^
      "\n@   4:    :F:  $norm0 = *slice;" ^
      "\n@   6:    :F:  $norm1 = Int+;" ^
      "\n@   8:    :F:  $norm2 = Int+;" ^
      "\n@  10:    :F:  $norm3 = None;" ^
      "\n@  12:    :F:  $norm4 = $norm0($norm1, $norm2, $norm3);" ^
      "\n@  14:    :F:  $simp2 = $norm4;" ^
      "\n@  18:  15:F:  $norm6 = $simp0.__getitem__;" ^
      "\n@  20:  15:F:  $norm7 = $norm6($simp1);" ^
      "\n@  22:  15:F:  $norm8 = $norm7.__iadd__;" ^
      "\n@  24:  15:F:  $simp5 = $norm8;" ^
      "\n@  26:  15:F:  $simp1 = $simp5;" ^
      "\n@  56:    :F:  goto 16;" ^
      "\n@  15:    :F:  catch $norm5;" ^
      "\n@  28:    :F:  $norm9 = *type;" ^
      "\n@  30:    :F:  $norm10 = $norm9($norm5);" ^
      "\n@  32:    :F:  $norm11 = *AttributeError;" ^
      "\n@  34:    :F:  $norm12 = $norm10.__eq__;" ^
      "\n@  36:    :F:  $norm13 = $norm12($norm11);" ^
      "\n@  38:    :F:  $norm14 = *bool;" ^
      "\n@  40:    :F:  $norm15 = $norm14($norm13);" ^
      "\n@  55:    :F:  goto 54 if not $norm15;" ^
      "\n@  42:    :F:  $norm16 = $simp0.__getitem__;" ^
      "\n@  44:    :F:  $norm17 = $norm16($simp1);" ^
      "\n@  46:    :F:  $norm18 = $norm17.__add__;" ^
      "\n@  48:    :F:  $simp4 = $norm18;" ^
      "\n@  50:    :F:  $simp1 = $simp4;" ^
      "\n@  53:    :F:  goto 52;" ^
      "\n@  54:    :F:  pass;" ^
      "\n@  51:    :F:  raise $norm5;" ^
      "\n@  52:    :F:  pass;" ^
      "\n@  16:    :F:  pass;" ^
      "\n@  58:    :F:  $norm19 = Int+;" ^
      "\n@  60:    :F:  $norm20 = $simp1($norm19);" ^
      "\n@  62:    :F:  $simp3 = $norm20;" ^
      "\n@  64:    :F:  $norm21 = $simp0.__setitem__;" ^
      "\n@  66:    :F:  $norm22 = $norm21($simp1, $simp3);"
    end
   ;;

   let multiassign_test = gen_module_test "multiassign_test"
    "x = y = 5"
    ("@   2:    :F:  $norm0 = Int+;" ^
     "\n@   4:    :F:  $simp0 = $norm0;" ^
     "\n@   6:    :F:  x = $simp0;" ^
     "\n@   8:    :F:  y = $simp0;")
   ;;

   let assign_to_member_test = gen_module_test "assign_to_member_test"
    "x.mem = 5"
    ("@   2:    :F:  $norm0 = Int+;" ^
     "\n@   4:    :F:  $simp0 = $norm0;" ^
     "\n@   6:    :F:  $norm1 = x.__setattr__;" ^
     "\n@   8:    :F:  $norm2 = \"mem\";" ^
     "\n@  10:    :F:  $norm3 = $norm1($norm2, $simp0);")
   ;;

   let assign_to_index_test = gen_module_test "assign_to_index_test"
    "list[5] = 5"
    ("@   2:    :F:  $norm0 = Int+;" ^
     "\n@   4:    :F:  $simp0 = $norm0;" ^
     "\n@   6:    :F:  $norm1 = list.__setitem__;" ^
     "\n@   8:    :F:  $norm2 = Int+;" ^
     "\n@  10:    :F:  $norm3 = $norm1($norm2, $simp0);")
   ;;

   let assign_to_slice_test = gen_module_test "assign_to_slice_test"
    "list[1:2] = 5"
    ("@   2:    :F:  $norm0 = Int+;" ^
     "\n@   4:    :F:  $simp0 = $norm0;" ^
     "\n@   6:    :F:  $norm1 = list.__setitem__;" ^
     "\n@   8:    :F:  $norm2 = *slice;" ^
     "\n@  10:    :F:  $norm3 = Int+;" ^
     "\n@  12:    :F:  $norm4 = Int+;" ^
     "\n@  14:    :F:  $norm5 = None;" ^
     "\n@  16:    :F:  $norm6 = $norm2($norm3, $norm4, $norm5);" ^
     "\n@  18:    :F:  $norm7 = $norm1($norm6, $simp0);")
   ;;

   let assign_from_tuple_test = gen_module_test "assign_from_tuple_test"
    "x,y = (1,2)"
    begin
      "@   2:    :F:  $norm0 = Int+;" ^
      "\n@   4:    :F:  $norm1 = Int+;" ^
      "\n@   6:    :F:  $norm2 = ($norm0, $norm1);" ^
      "\n@   8:    :F:  $simp0 = $norm2;" ^
      "\n@  10:    :F:  $norm3 = $simp0.__iter__;" ^
      "\n@  12:    :F:  $norm4 = $norm3();" ^
      "\n@  14:    :F:  $norm5 = $norm4.next;" ^
      "\n@  16:    :F:  $simp1 = $norm5;" ^
      "\n@  20:  17:F:  $norm7 = $simp1();" ^
      "\n@  22:  17:F:  $simp2 = $norm7;" ^
      "\n@  24:  17:F:  $norm8 = $simp1();" ^
      "\n@  26:  17:F:  $simp3 = $norm8;" ^
      "\n@  30:  27:F:  $norm10 = $simp1();" ^
      "\n@  32:  27:F:  $norm11 = *ValueError;" ^
      "\n@  34:  27:F:  $norm12 = \"too many values to unpack\";" ^
      "\n@  36:  27:F:  $norm13 = $norm11($norm12);" ^
      "\n@  37:  27:F:  raise $norm13;" ^
      "\n@  56:  17:F:  goto 28;" ^
      "\n@  27:  17:F:  catch $norm9;" ^
      "\n@  39:  17:F:  $norm14 = *type;" ^
      "\n@  41:  17:F:  $norm15 = $norm14($norm9);" ^
      "\n@  43:  17:F:  $norm16 = $norm15.__eq__;" ^
      "\n@  45:  17:F:  $norm17 = $norm16(StopIteration);" ^
      "\n@  47:  17:F:  $norm18 = *bool;" ^
      "\n@  49:  17:F:  $norm19 = $norm18($norm17);" ^
      "\n@  55:  17:F:  goto 54 if not $norm19;" ^
      "\n@  50:  17:F:  pass;" ^
      "\n@  53:  17:F:  goto 52;" ^
      "\n@  54:  17:F:  pass;" ^
      "\n@  51:  17:F:  raise $norm9;" ^
      "\n@  52:  17:F:  pass;" ^
      "\n@  28:  17:F:  pass;" ^
      "\n@  81:    :F:  goto 18;" ^
      "\n@  17:    :F:  catch $norm6;" ^
      "\n@  58:    :F:  $norm20 = *type;" ^
      "\n@  60:    :F:  $norm21 = $norm20($norm6);" ^
      "\n@  62:    :F:  $norm22 = $norm21.__eq__;" ^
      "\n@  64:    :F:  $norm23 = $norm22(StopIteration);" ^
      "\n@  66:    :F:  $norm24 = *bool;" ^
      "\n@  68:    :F:  $norm25 = $norm24($norm23);" ^
      "\n@  80:    :F:  goto 79 if not $norm25;" ^
      "\n@  70:    :F:  $norm26 = *ValueError;" ^
      "\n@  72:    :F:  $norm27 = \"too few values to unpack\";" ^
      "\n@  74:    :F:  $norm28 = $norm26($norm27);" ^
      "\n@  75:    :F:  raise $norm28;" ^
      "\n@  78:    :F:  goto 77;" ^
      "\n@  79:    :F:  pass;" ^
      "\n@  76:    :F:  raise $norm6;" ^
      "\n@  77:    :F:  pass;" ^
      "\n@  18:    :F:  pass;" ^
      "\n@  83:    :F:  $simp4 = $simp2;" ^
      "\n@  85:    :F:  x = $simp4;" ^
      "\n@  87:    :F:  $simp5 = $simp3;" ^
      "\n@  89:    :F:  y = $simp5;"

    end
   ;;

   let assignment_tests =
   [
    assign_test;
    augassign_test;
    augassign_to_member_test;
    augassign_to_list_test;
    multiassign_test;
    assign_to_member_test;
    assign_to_index_test;
    assign_to_slice_test;
    assign_from_tuple_test;
   ]
   ;;

   let unop_plus_test = gen_module_test "unop_plus_test"
    "+x"
    ("@   2:    :F:  $norm0 = x.__pos__;" ^
     "\n@   4:    :F:  $norm1 = $norm0();")
   ;;

   let unop_minus_test = gen_module_test "unop_minus_test"
    "-x"
    ("@   2:    :F:  $norm0 = x.__neg__;" ^
     "\n@   4:    :F:  $norm1 = $norm0();")
   ;;

   let unop_not_test = gen_module_test "unop_not_test"
    "not x"
    ("@   2:    :F:  $norm1 = *bool;" ^
     "\n@   4:    :F:  $norm2 = $norm1(x);" ^
     "\n@  16:    :F:  goto 15 if not $norm2;" ^
     "\n@   6:    :F:  $norm3 = false;" ^
     "\n@   8:    :F:  $norm0 = $norm3;" ^
     "\n@  14:    :F:  goto 13;" ^
     "\n@  15:    :F:  pass;" ^
     "\n@  10:    :F:  $norm4 = true;" ^
     "\n@  12:    :F:  $norm0 = $norm4;" ^
     "\n@  13:    :F:  pass;")
   ;;

   let gen_binop_test (name : string) (opstring : string) (opfunc : string ) =
   gen_module_test name ("x" ^ opstring ^ "y")
    begin
      "@   2:    :F:  $norm0 = x." ^ opfunc ^ ";" ^
      "\n@   4:    :F:  $norm1 = $norm0(y);"
    end
   ;;

   let boolop_and_test = gen_module_test "boolop_and_test"
    "x and 5 and True"
    ("@   2:    :F:  $norm1 = *bool;" ^
     "\n@   4:    :F:  $norm2 = $norm1(x);" ^
     "\n@  28:    :F:  goto 27 if not $norm2;" ^
     "\n@   6:    :F:  $norm3 = Int+;" ^
     "\n@   8:    :F:  $norm5 = *bool;" ^
     "\n@  10:    :F:  $norm6 = $norm5($norm3);" ^
     "\n@  20:    :F:  goto 19 if not $norm6;" ^
     "\n@  12:    :F:  $norm7 = true;" ^
     "\n@  14:    :F:  $norm4 = $norm7;" ^
     "\n@  18:    :F:  goto 17;" ^
     "\n@  19:    :F:  pass;" ^
     "\n@  16:    :F:  $norm4 = $norm3;" ^
     "\n@  17:    :F:  pass;" ^
     "\n@  22:    :F:  $norm0 = $norm4;" ^
     "\n@  26:    :F:  goto 25;" ^
     "\n@  27:    :F:  pass;" ^
     "\n@  24:    :F:  $norm0 = x;" ^
     "\n@  25:    :F:  pass;")
   ;;

   let boolop_or_test = gen_module_test "boolop_or_test"
    "x or 5 or True"
    ("@   2:    :F:  $norm1 = *bool;" ^
     "\n@   4:    :F:  $norm2 = $norm1(x);" ^
     "\n@  28:    :F:  goto 27 if not $norm2;" ^
     "\n@   6:    :F:  $norm0 = x;" ^
     "\n@  26:    :F:  goto 25;" ^
     "\n@  27:    :F:  pass;" ^
     "\n@   8:    :F:  $norm3 = Int+;" ^
     "\n@  10:    :F:  $norm5 = *bool;" ^
     "\n@  12:    :F:  $norm6 = $norm5($norm3);" ^
     "\n@  22:    :F:  goto 21 if not $norm6;" ^
     "\n@  14:    :F:  $norm4 = $norm3;" ^
     "\n@  20:    :F:  goto 19;" ^
     "\n@  21:    :F:  pass;" ^
     "\n@  16:    :F:  $norm7 = true;" ^
     "\n@  18:    :F:  $norm4 = $norm7;" ^
     "\n@  19:    :F:  pass;" ^
     "\n@  24:    :F:  $norm0 = $norm4;" ^
     "\n@  25:    :F:  pass;")
   ;;

   let gen_cmpop_test (name : string) (opstring : string) (opfunc : string ) =
   gen_module_test name ("x " ^ opstring ^ " y")
    begin
      "@   2:    :F:  $norm0 = x." ^ opfunc ^ ";" ^
      "\n@   4:    :F:  $norm1 = $norm0(y);"
    end
   ;;

   let multi_compare_test = gen_module_test "multi_compare_test"
    "x < y < z"
    ("@   2:    :F:  $norm0 = x.__lt__;" ^
     "\n@   4:    :F:  $norm1 = $norm0(y);" ^
     "\n@   6:    :F:  $norm3 = *bool;" ^
     "\n@   8:    :F:  $norm4 = $norm3($norm1);" ^
     "\n@  20:    :F:  goto 19 if not $norm4;" ^
     "\n@  10:    :F:  $norm5 = y.__lt__;" ^
     "\n@  12:    :F:  $norm6 = $norm5(z);" ^
     "\n@  14:    :F:  $norm2 = $norm6;" ^
     "\n@  18:    :F:  goto 17;" ^
     "\n@  19:    :F:  pass;" ^
     "\n@  16:    :F:  $norm2 = $norm1;" ^
     "\n@  17:    :F:  pass;")
   ;;

   let operator_tests =
   [
    unop_plus_test;
    unop_minus_test;
    unop_not_test;
    gen_binop_test "add_test" "+" "__add__";
    gen_binop_test "sub_test" "-" "__sub__";
    gen_binop_test "mul_test" "*" "__mul__";
    gen_binop_test "div_test" "/" "__div__";
    gen_binop_test "mod_test" "%" "__mod__";
    gen_binop_test "pow_test" "**" "__pow__";
    boolop_and_test;
    boolop_or_test;
    gen_cmpop_test "eq_test" "==" "__eq__";
    gen_cmpop_test "ne_test" "!=" "__ne__";
    gen_cmpop_test "lt_test" "<"  "__lt__";
    gen_cmpop_test "le_test" "<=" "__le__";
    gen_cmpop_test "gt_test" ">"  "__gt__";
    gen_cmpop_test "ge_test" ">=" "__ge__";
    gen_cmpop_test "in_test" "in" "__contains__";
    multi_compare_test;
   ]
   ;;

   let list_test = gen_module_test "list_test"
    "[1,0.0,'foo',1-9]"
    ("@   2:    :F:  $norm0 = Int+;" ^
     "\n@   4:    :F:  $norm1 = Float0;" ^
     "\n@   6:    :F:  $norm2 = \"foo\";" ^
     "\n@   8:    :F:  $norm3 = Int+;" ^
     "\n@  10:    :F:  $norm4 = $norm3.__sub__;" ^
     "\n@  12:    :F:  $norm5 = Int+;" ^
     "\n@  14:    :F:  $norm6 = $norm4($norm5);" ^
     "\n@  16:    :F:  $norm7 = [$norm0, $norm1, $norm2, $norm6];")

   ;;

   let tuple_test = gen_module_test "tuple_test"
    "(1,0.0,'foo',f(3+33))"
    ("@   2:    :F:  $norm0 = Int+;" ^
     "\n@   4:    :F:  $norm1 = Float0;" ^
     "\n@   6:    :F:  $norm2 = \"foo\";" ^
     "\n@   8:    :F:  $norm3 = Int+;" ^
     "\n@  10:    :F:  $norm4 = $norm3.__add__;" ^
     "\n@  12:    :F:  $norm5 = Int+;" ^
     "\n@  14:    :F:  $norm6 = $norm4($norm5);" ^
     "\n@  16:    :F:  $norm7 = f($norm6);" ^
     "\n@  18:    :F:  $norm8 = ($norm0, $norm1, $norm2, $norm7);")

   ;;

   let index_test = gen_module_test "index_test"
    "list[0]"
    ("@   2:    :F:  $norm0 = list.__getitem__;" ^
     "\n@   4:    :F:  $norm1 = Int0;" ^
     "\n@   6:    :F:  $norm2 = $norm0($norm1);")
   ;;

   let slice_test = gen_module_test "slice_test"
    "list[1:2]"
    ("@   2:    :F:  $norm0 = list.__getitem__;" ^
     "\n@   4:    :F:  $norm1 = *slice;" ^
     "\n@   6:    :F:  $norm2 = Int+;" ^
     "\n@   8:    :F:  $norm3 = Int+;" ^
     "\n@  10:    :F:  $norm4 = None;" ^
     "\n@  12:    :F:  $norm5 = $norm1($norm2, $norm3, $norm4);" ^
     "\n@  14:    :F:  $norm6 = $norm0($norm5);")
   ;;

   let if_test = gen_module_test "if_test"
    "if x: z = 1\nelif y: z = -1\nelse: 0"
    begin
      "@   2:    :F:  $norm0 = *bool;" ^
      "\n@   4:    :F:  $norm1 = $norm0(x);" ^
      "\n@  30:    :F:  goto 29 if not $norm1;" ^
      "\n@   6:    :F:  $norm2 = Int+;" ^
      "\n@   8:    :F:  $simp1 = $norm2;" ^
      "\n@  10:    :F:  z = $simp1;" ^
      "\n@  28:    :F:  goto 27;" ^
      "\n@  29:    :F:  pass;" ^
      "\n@  12:    :F:  $norm3 = *bool;" ^
      "\n@  14:    :F:  $norm4 = $norm3(y);" ^
      "\n@  26:    :F:  goto 25 if not $norm4;" ^
      "\n@  16:    :F:  $norm5 = Int-;" ^
      "\n@  18:    :F:  $simp0 = $norm5;" ^
      "\n@  20:    :F:  z = $simp0;" ^
      "\n@  24:    :F:  goto 23;" ^
      "\n@  25:    :F:  pass;" ^
      "\n@  22:    :F:  $norm6 = Int0;" ^
      "\n@  23:    :F:  pass;" ^
      "\n@  27:    :F:  pass;"
    end
   ;;

   let ifexp_test = gen_module_test "ifexp_test"
    "z = 1 if x else -1 if y else 0"
    begin
      "@   2:    :F:  $norm1 = *bool;" ^
      "\n@   4:    :F:  $norm2 = $norm1(x);" ^
      "\n@  30:    :F:  goto 29 if not $norm2;" ^
      "\n@   6:    :F:  $norm3 = Int+;" ^
      "\n@   8:    :F:  $norm0 = $norm3;" ^
      "\n@  28:    :F:  goto 27;" ^
      "\n@  29:    :F:  pass;" ^
      "\n@  10:    :F:  $norm5 = *bool;" ^
      "\n@  12:    :F:  $norm6 = $norm5(y);" ^
      "\n@  24:    :F:  goto 23 if not $norm6;" ^
      "\n@  14:    :F:  $norm7 = Int-;" ^
      "\n@  16:    :F:  $norm4 = $norm7;" ^
      "\n@  22:    :F:  goto 21;" ^
      "\n@  23:    :F:  pass;" ^
      "\n@  18:    :F:  $norm8 = Int0;" ^
      "\n@  20:    :F:  $norm4 = $norm8;" ^
      "\n@  21:    :F:  pass;" ^
      "\n@  26:    :F:  $norm0 = $norm4;" ^
      "\n@  27:    :F:  pass;" ^
      "\n@  32:    :F:  $simp0 = $norm0;" ^
      "\n@  34:    :F:  z = $simp0;"
    end
   ;;

   let tryexcept_test = gen_module_test "tryexcept_test"
    begin
      "try:" ^
      "\n  x = 5" ^
      "\n  somefunction()" ^
      "\nexcept ValueError as e:" ^
      "\n  pass" ^
      "\nexcept Int:" ^
      "\n  print 'got an int'"
    end
    begin
      "@   4:   1:F:  $norm1 = Int+;" ^
      "\n@   6:   1:F:  $simp0 = $norm1;" ^
      "\n@   8:   1:F:  x = $simp0;" ^
      "\n@  10:   1:F:  $norm2 = somefunction();" ^
      "\n@  50:    :F:  goto 2;" ^
      "\n@   1:    :F:  catch $norm0;" ^
      "\n@  12:    :F:  $norm3 = *type;" ^
      "\n@  14:    :F:  $norm4 = $norm3($norm0);" ^
      "\n@  16:    :F:  $norm5 = $norm4.__eq__;" ^
      "\n@  18:    :F:  $norm6 = $norm5(ValueError);" ^
      "\n@  20:    :F:  $norm7 = *bool;" ^
      "\n@  22:    :F:  $norm8 = $norm7($norm6);" ^
      "\n@  49:    :F:  goto 48 if not $norm8;" ^
      "\n@  24:    :F:  e = $norm0;" ^
      "\n@  25:    :F:  pass;" ^
      "\n@  47:    :F:  goto 46;" ^
      "\n@  48:    :F:  pass;" ^
      "\n@  27:    :F:  $norm9 = *type;" ^
      "\n@  29:    :F:  $norm10 = $norm9($norm0);" ^
      "\n@  31:    :F:  $norm11 = $norm10.__eq__;" ^
      "\n@  33:    :F:  $norm12 = $norm11(Int);" ^
      "\n@  35:    :F:  $norm13 = *bool;" ^
      "\n@  37:    :F:  $norm14 = $norm13($norm12);" ^
      "\n@  45:    :F:  goto 44 if not $norm14;" ^
      "\n@  39:    :F:  $norm15 = \"got an int\";" ^
      "\n@  40:    :F:  print $norm15;" ^
      "\n@  43:    :F:  goto 42;" ^
      "\n@  44:    :F:  pass;" ^
      "\n@  41:    :F:  raise $norm0;" ^
      "\n@  42:    :F:  pass;" ^
      "\n@  46:    :F:  pass;" ^
      "\n@   2:    :F:  pass;"
    end
   ;;

   let funcdef_test = gen_module_test "funcdef_test"
    begin
      "def f():" ^
      "\n  x = f" ^
      "\n  return n" ^
      "\nx = f()"
    end
    begin
      "@   7:    :F:  $norm0 = fun() {" ^
      "\n@   2:    :F:    $simp0 = f;" ^
      "\n@   4:    :F:    f$1_x = $simp0;" ^
      "\n@   5:    :F:    return n;" ^
      "\n};" ^
      "\n@   9:    :F:  f = $norm0;" ^
      "\n@  11:    :F:  $norm1 = f();" ^
      "\n@  13:    :F:  $simp1 = $norm1;" ^
      "\n@  15:    :F:  x = $simp1;"
    end
   ;;

   let funcdef_args_test = gen_module_test "funcdef_args_test"
    begin
      "def f(x,y):" ^
      "\n  x = f" ^
      "\n  return n" ^
      "\nx = f(z)"
    end
    begin
      "@   7:    :F:  $norm0 = fun(f$1_x, f$1_y) {" ^
      "\n@   2:    :F:    $simp0 = f;" ^
      "\n@   4:    :F:    f$1_x = $simp0;" ^
      "\n@   5:    :F:    return n;" ^
      "\n};" ^
      "\n@   9:    :F:  f = $norm0;" ^
      "\n@  11:    :F:  $norm1 = f(z);" ^
      "\n@  13:    :F:  $simp1 = $norm1;" ^
      "\n@  15:    :F:  x = $simp1;"
    end
   ;;

   let while_test = gen_module_test "while_test"
    "while x < 3:\n\tx = x + 1"
    ("@  11:    :T:  pass;" ^
     "\n@   2:    :T:  $norm0 = Int+;" ^
     "\n@   4:    :T:  $norm1 = x.__lt__;" ^
     "\n@   6:    :T:  $norm2 = $norm1($norm0);" ^
     "\n@   8:    :T:  $norm3 = *bool;" ^
     "\n@  10:    :T:  $norm4 = $norm3($norm2);" ^
     "\n@  23:    :T:  goto 12 if not $norm4;" ^
     "\n@  14:    :T:  $norm5 = x.__add__;" ^
     "\n@  16:    :T:  $norm6 = Int+;" ^
     "\n@  18:    :T:  $norm7 = $norm5($norm6);" ^
     "\n@  20:    :T:  $simp0 = $norm7;" ^
     "\n@  22:    :T:  x = $simp0;" ^
     "\n@  24:    :T:  goto 11;" ^
     "\n@  12:    :T:  pass;")

   ;;

   let break_test = gen_module_test "break_test"
    "while x < 3:\n\tbreak"
    ("@  11:    :T:  pass;" ^
     "\n@   2:    :T:  $norm0 = Int+;" ^
     "\n@   4:    :T:  $norm1 = x.__lt__;" ^
     "\n@   6:    :T:  $norm2 = $norm1($norm0);" ^
     "\n@   8:    :T:  $norm3 = *bool;" ^
     "\n@  10:    :T:  $norm4 = $norm3($norm2);" ^
     "\n@  14:    :T:  goto 12 if not $norm4;" ^
     "\n@  13:    :T:  goto 12;" ^
     "\n@  15:    :T:  goto 11;" ^
     "\n@  12:    :T:  pass;")
   ;;

   let continue_test = gen_module_test "continue_test"
    "while x < 3:\n\tcontinue"
    ("@  11:    :T:  pass;" ^
     "\n@   2:    :T:  $norm0 = Int+;" ^
     "\n@   4:    :T:  $norm1 = x.__lt__;" ^
     "\n@   6:    :T:  $norm2 = $norm1($norm0);" ^
     "\n@   8:    :T:  $norm3 = *bool;" ^
     "\n@  10:    :T:  $norm4 = $norm3($norm2);" ^
     "\n@  14:    :T:  goto 12 if not $norm4;" ^
     "\n@  13:    :T:  goto 11;" ^
     "\n@  15:    :T:  goto 11;" ^
     "\n@  12:    :T:  pass;")
   ;;

   let for_test = gen_module_test "for_test"
    begin
      "for i in [-1,1]:" ^
      "\n  if i < 3:" ^
      "\n    print i"
    end
    begin
      "@   2:    :F:  $norm0 = Int-;" ^
      "\n@   4:    :F:  $norm1 = Int+;" ^
      "\n@   6:    :F:  $norm2 = [$norm0, $norm1];" ^
      "\n@   8:    :F:  $norm3 = $norm2.__iter__;" ^
      "\n@  10:    :F:  $norm4 = $norm3();" ^
      "\n@  12:    :F:  $norm5 = $norm4.next;" ^
      "\n@  14:    :F:  $simp1 = $norm5;" ^
      "\n@  16:    :F:  $simp0 = $simp1;" ^
      "\n@  25:  17:T:  pass;" ^
      "\n@  20:  17:T:  $norm7 = true;" ^
      "\n@  22:  17:T:  $norm8 = *bool;" ^
      "\n@  24:  17:T:  $norm9 = $norm8($norm7);" ^
      "\n@  48:  17:T:  goto 26 if not $norm9;" ^
      "\n@  28:  17:T:  $norm10 = $simp0();" ^
      "\n@  30:  17:T:  $simp2 = $norm10;" ^
      "\n@  32:  17:T:  i = $simp2;" ^
      "\n@  34:  17:T:  $norm11 = Int+;" ^
      "\n@  36:  17:T:  $norm12 = i.__lt__;" ^
      "\n@  38:  17:T:  $norm13 = $norm12($norm11);" ^
      "\n@  40:  17:T:  $norm14 = *bool;" ^
      "\n@  42:  17:T:  $norm15 = $norm14($norm13);" ^
      "\n@  47:  17:T:  goto 46 if not $norm15;" ^
      "\n@  43:  17:T:  print i;" ^
      "\n@  45:  17:T:  goto 44;" ^
      "\n@  46:  17:T:  pass;" ^
      "\n@  44:  17:T:  pass;" ^
      "\n@  49:  17:T:  goto 25;" ^
      "\n@  26:  17:T:  pass;" ^
      "\n@  68:    :F:  goto 18;" ^
      "\n@  17:    :F:  catch $norm6;" ^
      "\n@  51:    :F:  $norm16 = *type;" ^
      "\n@  53:    :F:  $norm17 = $norm16($norm6);" ^
      "\n@  55:    :F:  $norm18 = $norm17.__eq__;" ^
      "\n@  57:    :F:  $norm19 = $norm18(StopIteration);" ^
      "\n@  59:    :F:  $norm20 = *bool;" ^
      "\n@  61:    :F:  $norm21 = $norm20($norm19);" ^
      "\n@  67:    :F:  goto 66 if not $norm21;" ^
      "\n@  62:    :F:  pass;" ^
      "\n@  65:    :F:  goto 64;" ^
      "\n@  66:    :F:  pass;" ^
      "\n@  63:    :F:  raise $norm6;" ^
      "\n@  64:    :F:  pass;" ^
      "\n@  18:    :F:  pass;"
    end
   ;;*)

let tests =
  "test_abstract_ast">:::
  literal_tests (*@
                  stmt_tests @
                  operator_tests @
                  assignment_tests @
                  [
                  attribute_test;
                  call_test;
                  call_args_test;
                  list_test;
                  tuple_test;
                  index_test;
                  slice_test;
                  if_test;
                  ifexp_test;
                  tryexcept_test;
                  funcdef_test;
                  funcdef_args_test;
                  while_test;
                  break_test;
                  continue_test;
                  for_test;
                  ] *)
;;
