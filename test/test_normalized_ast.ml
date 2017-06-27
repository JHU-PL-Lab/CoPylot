open OUnit2;;
open Batteries;;
open Jhupllib;;
open Pp_utils;;

open Python2_ast_types;;
open Python2_ast_pipeline;;
open Python2_normalized_ast;;
open Python2_normalized_ast_pretty;;

let string_of_modl m = Pp_utils.pp_to_string pp_modl m;;

let parse_to_normalized_safe prog short_names =
  try
    parse_to_normalized prog short_names
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
  "\n@   6:   1:T:    $norm2 = builtin_type;" ^
  "\n@   8:   1:T:    $norm3 = $norm2($norm0);" ^
  "\n@  10:   1:T:    $norm4 = Builtin_method_wrapper_type;" ^
  "\n@  12:   1:T:    $norm5 = $norm3 is $norm4;" ^
  "\n@  14:   1:T:    $norm8 = builtin_bool;" ^
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
  "\n@  32:   1:T:    $norm12 = builtin_bool;" ^
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
  "\n@  52:   1:T:    $norm24 = builtin_type;" ^
  "\n@  54:   1:T:    $norm25 = $norm24($norm18);" ^
  "\n@  56:   1:T:    $norm26 = builtin_AttributeError;" ^
  "\n@  58:   1:T:    $norm27 = $norm25 is $norm26;" ^
  "\n@  60:   1:T:    $norm28 = builtin_bool;" ^
  "\n@  62:   1:T:    $norm29 = $norm28($norm27);" ^
  "\n@ 104:   1:T:    goto 103 if not $norm29;" ^
  "\n@  64:   1:T:    $norm16 = $norm18;" ^
  "\n@  68:  65:T:    $norm31 = $norm0.__getattr__;" ^
  "\n@  70:  65:T:    $norm15 = $norm31;" ^
  "\n@  89:   1:T:    goto 66;" ^
  "\n@  65:   1:T:    catch $norm30;" ^
  "\n@  72:   1:T:    $norm32 = builtin_type;" ^
  "\n@  74:   1:T:    $norm33 = $norm32($norm30);" ^
  "\n@  76:   1:T:    $norm34 = builtin_AttributeError;" ^
  "\n@  78:   1:T:    $norm35 = $norm33 is $norm34;" ^
  "\n@  80:   1:T:    $norm36 = builtin_bool;" ^
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
  "\n@ 111:    :F:    $norm42 = builtin_type;" ^
  "\n@ 113:    :F:    $norm43 = $norm42($norm1);" ^
  "\n@ 115:    :F:    $norm44 = builtin_AttributeError;" ^
  "\n@ 117:    :F:    $norm45 = $norm43 is $norm44;" ^
  "\n@ 119:    :F:    $norm46 = builtin_bool;" ^
  "\n@ 121:    :F:    $norm47 = $norm46($norm45);" ^
  "\n@ 133:    :F:    goto 132 if not $norm47;" ^
  "\n@ 123:    :F:    $norm48 = builtin_TypeError;" ^
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
    (expected : string) =
  name>::
  ( fun _ ->
      let actual = parse_to_normalized_safe prog true in
      Python2_ast_simplifier.reset_unique_name ();
      Python2_ast_normalizer.reset_unique_name ();

      let distinct_uids = verify_unique_uids actual in
      assert_bool ("Repeated UIDs:\n" ^ string_of_modl actual) distinct_uids;

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
    gen_literal_test "int+_test" "4" "4";
    gen_literal_test "int0_test" "0" "0";
    gen_literal_test "int-_test" "-4" "-4";

    gen_literal_test "float+_test" "4.0" "4.000000";
    gen_literal_test "float0_test" "0.0" "0.000000";
    gen_literal_test "float-_test" "-4.0" "-4.000000";

    gen_literal_test "string_test" "'foo'" "\"foo\"";

    gen_literal_test "bool_true_test" "True" "true";
    gen_literal_test "bool_false_test" "False" "false";

    gen_literal_test "none_test" "None" "*None";
  ]
;;

let stmt_tests =
  [
    gen_module_test "return_var_test" "return x" "@ 140:    :F:  return x;";

    gen_module_test "return_int_test" "return 5"
      "@ 141:    :F:  $norm52 = 5;\n@ 142:    :F:  return $norm52;";

    gen_module_test "return_none_test" "return"
      "@ 140:    :F:  return *None;";


    gen_module_test "print_test1" "print x" "@ 140:    :F:  print x;";
    gen_module_test "print_test2" "print x,y" "@ 140:    :F:  print x, y;";

    gen_module_test "raise_test" "raise x" "@ 140:    :F:  raise x;";

    gen_module_test "pass_test" "pass" "@ 140:    :F:  pass;";
  ]
;;

let attribute_test = gen_module_test "attribute_test"
    "x.mem"
    begin
      "@ 143: 140:F:  $norm57 = x.__getattribute__;" ^
      "\n@ 145: 140:F:  $norm52 = $norm57;" ^
      "\n@ 147: 140:F:  $norm58 = \"mem\";" ^
      "\n@ 149: 140:F:  $norm60 = *get_call($norm52);" ^
      "\n@ 151: 140:F:  $norm59 = $norm60;" ^
      "\n@ 153: 140:F:  $norm61 = $norm59($norm58);" ^
      "\n@ 155: 140:F:  $norm55 = $norm61;" ^
      "\n@ 210:    :F:  goto 141;" ^
      "\n@ 140:    :F:  catch $norm56;" ^
      "\n@ 157:    :F:  $norm62 = builtin_type;" ^
      "\n@ 159:    :F:  $norm63 = $norm62($norm56);" ^
      "\n@ 161:    :F:  $norm64 = builtin_AttributeError;" ^
      "\n@ 163:    :F:  $norm65 = $norm63 is $norm64;" ^
      "\n@ 165:    :F:  $norm66 = builtin_bool;" ^
      "\n@ 167:    :F:  $norm67 = $norm66($norm65);" ^
      "\n@ 209:    :F:  goto 208 if not $norm67;" ^
      "\n@ 169:    :F:  $norm54 = $norm56;" ^
      "\n@ 173: 170:F:  $norm69 = x.__getattr__;" ^
      "\n@ 175: 170:F:  $norm53 = $norm69;" ^
      "\n@ 194:    :F:  goto 171;" ^
      "\n@ 170:    :F:  catch $norm68;" ^
      "\n@ 177:    :F:  $norm70 = builtin_type;" ^
      "\n@ 179:    :F:  $norm71 = $norm70($norm68);" ^
      "\n@ 181:    :F:  $norm72 = builtin_AttributeError;" ^
      "\n@ 183:    :F:  $norm73 = $norm71 is $norm72;" ^
      "\n@ 185:    :F:  $norm74 = builtin_bool;" ^
      "\n@ 187:    :F:  $norm75 = $norm74($norm73);" ^
      "\n@ 193:    :F:  goto 192 if not $norm75;" ^
      "\n@ 188:    :F:  raise $norm54;" ^
      "\n@ 191:    :F:  goto 190;" ^
      "\n@ 192:    :F:  pass;" ^
      "\n@ 189:    :F:  raise $norm68;" ^
      "\n@ 190:    :F:  pass;" ^
      "\n@ 171:    :F:  pass;" ^
      "\n@ 196:    :F:  $norm76 = \"mem\";" ^
      "\n@ 198:    :F:  $norm78 = *get_call($norm53);" ^
      "\n@ 200:    :F:  $norm77 = $norm78;" ^
      "\n@ 202:    :F:  $norm79 = $norm77($norm76);" ^
      "\n@ 204:    :F:  $norm55 = $norm79;" ^
      "\n@ 207:    :F:  goto 206;" ^
      "\n@ 208:    :F:  pass;" ^
      "\n@ 205:    :F:  raise $norm56;" ^
      "\n@ 206:    :F:  pass;" ^
      "\n@ 141:    :F:  pass;"
    end
;;

let call_test = gen_module_test "call_test"
    "foo()"
    begin
      "@ 141:    :F:  $norm53 = *get_call(foo);" ^
      "\n@ 143:    :F:  $norm52 = $norm53;" ^
      "\n@ 145:    :F:  $norm54 = $norm52();"
    end
;;

let call_args_test = gen_module_test "call_args_test"
    "foo(1,x)"
    begin
      "@ 141:    :F:  $norm52 = 1;" ^
      "\n@ 143:    :F:  $norm54 = *get_call(foo);" ^
      "\n@ 145:    :F:  $norm53 = $norm54;" ^
      "\n@ 147:    :F:  $norm55 = $norm53($norm52, x);"
    end

;;

let assign_test = gen_module_test "assign_test"
    "x = 5"
    begin
      "@ 141:    :F:  $norm52 = 5;" ^
      "\n@ 143:    :F:  $simp0 = $norm52;" ^
      "\n@ 145:    :F:  x = $simp0;"
    end
;;

let augassign_test = gen_module_test "augassign_test"
    "x += 5"
    begin
      "@   4:   1:F:  $norm1 = x.__iadd__;" ^
      "\n@   6:   1:F:  $simp4 = $norm1;" ^
      "\n@   8:   1:F:  $simp1 = $simp4;" ^
      "\n@  34:    :F:  goto 2;" ^
      "\n@   1:    :F:  catch $norm0;" ^
      "\n@  10:    :F:  $norm2 = builtin_type;" ^
      "\n@  12:    :F:  $norm3 = $norm2($norm0);" ^
      "\n@  14:    :F:  $norm4 = builtin_AttributeError;" ^
      "\n@  16:    :F:  $norm5 = $norm3.__eq__;" ^
      "\n@  18:    :F:  $norm6 = $norm5($norm4);" ^
      "\n@  20:    :F:  $norm7 = builtin_bool;" ^
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
      "\n@  36:    :F:  $norm10 = 5;" ^
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
      "\n@  14:    :F:  $norm3 = builtin_type;" ^
      "\n@  16:    :F:  $norm4 = $norm3($norm0);" ^
      "\n@  18:    :F:  $norm5 = builtin_AttributeError;" ^
      "\n@  20:    :F:  $norm6 = $norm4.__eq__;" ^
      "\n@  22:    :F:  $norm7 = $norm6($norm5);" ^
      "\n@  24:    :F:  $norm8 = builtin_bool;" ^
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
      "\n@  42:    :F:  $norm12 = 5;" ^
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
      "\n@   4:    :F:  $norm0 = builtin_slice;" ^
      "\n@   6:    :F:  $norm1 = 1;" ^
      "\n@   8:    :F:  $norm2 = 2;" ^
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
      "\n@  28:    :F:  $norm9 = builtin_type;" ^
      "\n@  30:    :F:  $norm10 = $norm9($norm5);" ^
      "\n@  32:    :F:  $norm11 = builtin_AttributeError;" ^
      "\n@  34:    :F:  $norm12 = $norm10.__eq__;" ^
      "\n@  36:    :F:  $norm13 = $norm12($norm11);" ^
      "\n@  38:    :F:  $norm14 = builtin_bool;" ^
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
      "\n@  58:    :F:  $norm19 = 5;" ^
      "\n@  60:    :F:  $norm20 = $simp1($norm19);" ^
      "\n@  62:    :F:  $simp3 = $norm20;" ^
      "\n@  64:    :F:  $norm21 = $simp0.__setitem__;" ^
      "\n@  66:    :F:  $norm22 = $norm21($simp1, $simp3);"
    end
;;

let multiassign_test = gen_module_test "multiassign_test"
    "x = y = 5"
    ("@   2:    :F:  $norm0 = 5;" ^
     "\n@   4:    :F:  $simp0 = $norm0;" ^
     "\n@   6:    :F:  x = $simp0;" ^
     "\n@   8:    :F:  y = $simp0;")
;;

let assign_to_member_test = gen_module_test "assign_to_member_test"
    "x.mem = 5"
    ("@   2:    :F:  $norm0 = 5;" ^
     "\n@   4:    :F:  $simp0 = $norm0;" ^
     "\n@   6:    :F:  $norm1 = x.__setattr__;" ^
     "\n@   8:    :F:  $norm2 = \"mem\";" ^
     "\n@  10:    :F:  $norm3 = $norm1($norm2, $simp0);")
;;

let assign_to_index_test = gen_module_test "assign_to_index_test"
    "list[5] = 5"
    ("@   2:    :F:  $norm0 = 5;" ^
     "\n@   4:    :F:  $simp0 = $norm0;" ^
     "\n@   6:    :F:  $norm1 = list.__setitem__;" ^
     "\n@   8:    :F:  $norm2 = 5;" ^
     "\n@  10:    :F:  $norm3 = $norm1($norm2, $simp0);")
;;

let assign_to_slice_test = gen_module_test "assign_to_slice_test"
    "list[1:2] = 5"
    ("@   2:    :F:  $norm0 = 5;" ^
     "\n@   4:    :F:  $simp0 = $norm0;" ^
     "\n@   6:    :F:  $norm1 = list.__setitem__;" ^
     "\n@   8:    :F:  $norm2 = builtin_slice;" ^
     "\n@  10:    :F:  $norm3 = 1;" ^
     "\n@  12:    :F:  $norm4 = 2;" ^
     "\n@  14:    :F:  $norm5 = None;" ^
     "\n@  16:    :F:  $norm6 = $norm2($norm3, $norm4, $norm5);" ^
     "\n@  18:    :F:  $norm7 = $norm1($norm6, $simp0);")
;;

let assign_from_tuple_test = gen_module_test "assign_from_tuple_test"
    "x,y = (1,2)"
    begin
      "@   2:    :F:  $norm0 = 1;" ^
      "\n@   4:    :F:  $norm1 = 2;" ^
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
      "\n@  32:  27:F:  $norm11 = builtin_ValueError;" ^
      "\n@  34:  27:F:  $norm12 = \"too many values to unpack\";" ^
      "\n@  36:  27:F:  $norm13 = $norm11($norm12);" ^
      "\n@  37:  27:F:  raise $norm13;" ^
      "\n@  56:  17:F:  goto 28;" ^
      "\n@  27:  17:F:  catch $norm9;" ^
      "\n@  39:  17:F:  $norm14 = builtin_type;" ^
      "\n@  41:  17:F:  $norm15 = $norm14($norm9);" ^
      "\n@  43:  17:F:  $norm16 = $norm15.__eq__;" ^
      "\n@  45:  17:F:  $norm17 = $norm16(StopIteration);" ^
      "\n@  47:  17:F:  $norm18 = builtin_bool;" ^
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
      "\n@  58:    :F:  $norm20 = builtin_type;" ^
      "\n@  60:    :F:  $norm21 = $norm20($norm6);" ^
      "\n@  62:    :F:  $norm22 = $norm21.__eq__;" ^
      "\n@  64:    :F:  $norm23 = $norm22(StopIteration);" ^
      "\n@  66:    :F:  $norm24 = builtin_bool;" ^
      "\n@  68:    :F:  $norm25 = $norm24($norm23);" ^
      "\n@  80:    :F:  goto 79 if not $norm25;" ^
      "\n@  70:    :F:  $norm26 = builtin_ValueError;" ^
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
    (* augassign_test;
    augassign_to_member_test;
    augassign_to_list_test;
    multiassign_test;
    assign_to_member_test;
    assign_to_index_test;
    assign_to_slice_test;
    assign_from_tuple_test; *)
  ]
;;

(* let unop_plus_test = gen_module_test "unop_plus_test"
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
    ("@   2:    :F:  $norm1 = builtin_bool;" ^
     "\n@   4:    :F:  $norm2 = $norm1(x);" ^
     "\n@  16:    :F:  goto 15 if not $norm2;" ^
     "\n@   6:    :F:  $norm3 = false;" ^
     "\n@   8:    :F:  $norm0 = $norm3;" ^
     "\n@  14:    :F:  goto 13;" ^
     "\n@  15:    :F:  pass;" ^
     "\n@  10:    :F:  $norm4 = true;" ^
     "\n@  12:    :F:  $norm0 = $norm4;" ^
     "\n@  13:    :F:  pass;")
   ;; *)

let gen_binop_test (name : string) (opstring : string) (opfunc : string ) =
  gen_module_test name ("x " ^ opstring ^ " y")
    begin
      "@ 143: 140:F:  $norm57 = x.__getattribute__;" ^
      "\n@ 145: 140:F:  $norm52 = $norm57;" ^
      "\n@ 147: 140:F:  $norm58 = \"" ^ opfunc ^ "\";" ^
      "\n@ 149: 140:F:  $norm60 = *get_call($norm52);" ^
      "\n@ 151: 140:F:  $norm59 = $norm60;" ^
      "\n@ 153: 140:F:  $norm61 = $norm59($norm58);" ^
      "\n@ 155: 140:F:  $norm55 = $norm61;" ^
      "\n@ 210:    :F:  goto 141;" ^
      "\n@ 140:    :F:  catch $norm56;" ^
      "\n@ 157:    :F:  $norm62 = builtin_type;" ^
      "\n@ 159:    :F:  $norm63 = $norm62($norm56);" ^
      "\n@ 161:    :F:  $norm64 = builtin_AttributeError;" ^
      "\n@ 163:    :F:  $norm65 = $norm63 is $norm64;" ^
      "\n@ 165:    :F:  $norm66 = builtin_bool;" ^
      "\n@ 167:    :F:  $norm67 = $norm66($norm65);" ^
      "\n@ 209:    :F:  goto 208 if not $norm67;" ^
      "\n@ 169:    :F:  $norm54 = $norm56;" ^
      "\n@ 173: 170:F:  $norm69 = x.__getattr__;" ^
      "\n@ 175: 170:F:  $norm53 = $norm69;" ^
      "\n@ 194:    :F:  goto 171;" ^
      "\n@ 170:    :F:  catch $norm68;" ^
      "\n@ 177:    :F:  $norm70 = builtin_type;" ^
      "\n@ 179:    :F:  $norm71 = $norm70($norm68);" ^
      "\n@ 181:    :F:  $norm72 = builtin_AttributeError;" ^
      "\n@ 183:    :F:  $norm73 = $norm71 is $norm72;" ^
      "\n@ 185:    :F:  $norm74 = builtin_bool;" ^
      "\n@ 187:    :F:  $norm75 = $norm74($norm73);" ^
      "\n@ 193:    :F:  goto 192 if not $norm75;" ^
      "\n@ 188:    :F:  raise $norm54;" ^
      "\n@ 191:    :F:  goto 190;" ^
      "\n@ 192:    :F:  pass;" ^
      "\n@ 189:    :F:  raise $norm68;" ^
      "\n@ 190:    :F:  pass;" ^
      "\n@ 171:    :F:  pass;" ^
      "\n@ 196:    :F:  $norm76 = \"" ^ opfunc ^ "\";" ^
      "\n@ 198:    :F:  $norm78 = *get_call($norm53);" ^
      "\n@ 200:    :F:  $norm77 = $norm78;" ^
      "\n@ 202:    :F:  $norm79 = $norm77($norm76);" ^
      "\n@ 204:    :F:  $norm55 = $norm79;" ^
      "\n@ 207:    :F:  goto 206;" ^
      "\n@ 208:    :F:  pass;" ^
      "\n@ 205:    :F:  raise $norm56;" ^
      "\n@ 206:    :F:  pass;" ^
      "\n@ 141:    :F:  pass;" ^
      "\n@ 212:    :F:  $norm81 = *get_call($norm55);" ^
      "\n@ 214:    :F:  $norm80 = $norm81;" ^
      "\n@ 216:    :F:  $norm82 = $norm80(y);"
    end
;;

let boolop_and_test = gen_module_test "boolop_and_test"
    "x and 5 and True"
    begin
      "@ 141:    :F:  $norm53 = builtin_bool;" ^
      "\n@ 143:    :F:  $norm54 = $norm53(x);" ^
      "\n@ 167:    :F:  goto 166 if not $norm54;" ^
      "\n@ 145:    :F:  $norm55 = 5;" ^
      "\n@ 147:    :F:  $norm57 = builtin_bool;" ^
      "\n@ 149:    :F:  $norm58 = $norm57($norm55);" ^
      "\n@ 159:    :F:  goto 158 if not $norm58;" ^
      "\n@ 151:    :F:  $norm59 = true;" ^
      "\n@ 153:    :F:  $norm56 = $norm59;" ^
      "\n@ 157:    :F:  goto 156;" ^
      "\n@ 158:    :F:  pass;" ^
      "\n@ 155:    :F:  $norm56 = $norm55;" ^
      "\n@ 156:    :F:  pass;" ^
      "\n@ 161:    :F:  $norm52 = $norm56;" ^
      "\n@ 165:    :F:  goto 164;" ^
      "\n@ 166:    :F:  pass;" ^
      "\n@ 163:    :F:  $norm52 = x;" ^
      "\n@ 164:    :F:  pass;"
    end
;;

let boolop_or_test = gen_module_test "boolop_or_test"
    "x or 5 or True"
    begin
      "@ 141:    :F:  $norm53 = builtin_bool;" ^
      "\n@ 143:    :F:  $norm54 = $norm53(x);" ^
      "\n@ 167:    :F:  goto 166 if not $norm54;" ^
      "\n@ 145:    :F:  $norm52 = x;" ^
      "\n@ 165:    :F:  goto 164;" ^
      "\n@ 166:    :F:  pass;" ^
      "\n@ 147:    :F:  $norm55 = 5;" ^
      "\n@ 149:    :F:  $norm57 = builtin_bool;" ^
      "\n@ 151:    :F:  $norm58 = $norm57($norm55);" ^
      "\n@ 161:    :F:  goto 160 if not $norm58;" ^
      "\n@ 153:    :F:  $norm56 = $norm55;" ^
      "\n@ 159:    :F:  goto 158;" ^
      "\n@ 160:    :F:  pass;" ^
      "\n@ 155:    :F:  $norm59 = true;" ^
      "\n@ 157:    :F:  $norm56 = $norm59;" ^
      "\n@ 158:    :F:  pass;" ^
      "\n@ 163:    :F:  $norm52 = $norm56;" ^
      "\n@ 164:    :F:  pass;"
    end
;;

let gen_cmpop_test (name : string) (opstring : string) (opfunc : string ) =
  (* gen_module_test name ("x " ^ opstring ^ " y")
     begin
      "@   2:    :F:  $norm0 = x." ^ opfunc ^ ";" ^
      "\n@   4:    :F:  $norm1 = $norm0(y);"
     end *)
  gen_binop_test name opstring opfunc
;;

let multi_compare_test = gen_module_test "multi_compare_test"
    "x < y < z"
    begin
      "@ 143: 140:F:  $norm57 = x.__getattribute__;" ^
      "\n@ 145: 140:F:  $norm52 = $norm57;" ^
      "\n@ 147: 140:F:  $norm58 = \"__lt__\";" ^
      "\n@ 149: 140:F:  $norm60 = *get_call($norm52);" ^
      "\n@ 151: 140:F:  $norm59 = $norm60;" ^
      "\n@ 153: 140:F:  $norm61 = $norm59($norm58);" ^
      "\n@ 155: 140:F:  $norm55 = $norm61;" ^
      "\n@ 210:    :F:  goto 141;" ^
      "\n@ 140:    :F:  catch $norm56;" ^
      "\n@ 157:    :F:  $norm62 = builtin_type;" ^
      "\n@ 159:    :F:  $norm63 = $norm62($norm56);" ^
      "\n@ 161:    :F:  $norm64 = builtin_AttributeError;" ^
      "\n@ 163:    :F:  $norm65 = $norm63 is $norm64;" ^
      "\n@ 165:    :F:  $norm66 = builtin_bool;" ^
      "\n@ 167:    :F:  $norm67 = $norm66($norm65);" ^
      "\n@ 209:    :F:  goto 208 if not $norm67;" ^
      "\n@ 169:    :F:  $norm54 = $norm56;" ^
      "\n@ 173: 170:F:  $norm69 = x.__getattr__;" ^
      "\n@ 175: 170:F:  $norm53 = $norm69;" ^
      "\n@ 194:    :F:  goto 171;" ^
      "\n@ 170:    :F:  catch $norm68;" ^
      "\n@ 177:    :F:  $norm70 = builtin_type;" ^
      "\n@ 179:    :F:  $norm71 = $norm70($norm68);" ^
      "\n@ 181:    :F:  $norm72 = builtin_AttributeError;" ^
      "\n@ 183:    :F:  $norm73 = $norm71 is $norm72;" ^
      "\n@ 185:    :F:  $norm74 = builtin_bool;" ^
      "\n@ 187:    :F:  $norm75 = $norm74($norm73);" ^
      "\n@ 193:    :F:  goto 192 if not $norm75;" ^
      "\n@ 188:    :F:  raise $norm54;" ^
      "\n@ 191:    :F:  goto 190;" ^
      "\n@ 192:    :F:  pass;" ^
      "\n@ 189:    :F:  raise $norm68;" ^
      "\n@ 190:    :F:  pass;" ^
      "\n@ 171:    :F:  pass;" ^
      "\n@ 196:    :F:  $norm76 = \"__lt__\";" ^
      "\n@ 198:    :F:  $norm78 = *get_call($norm53);" ^
      "\n@ 200:    :F:  $norm77 = $norm78;" ^
      "\n@ 202:    :F:  $norm79 = $norm77($norm76);" ^
      "\n@ 204:    :F:  $norm55 = $norm79;" ^
      "\n@ 207:    :F:  goto 206;" ^
      "\n@ 208:    :F:  pass;" ^
      "\n@ 205:    :F:  raise $norm56;" ^
      "\n@ 206:    :F:  pass;" ^
      "\n@ 141:    :F:  pass;" ^
      "\n@ 212:    :F:  $norm81 = *get_call($norm55);" ^
      "\n@ 214:    :F:  $norm80 = $norm81;" ^
      "\n@ 216:    :F:  $norm82 = $norm80(y);" ^
      "\n@ 218:    :F:  $norm84 = builtin_bool;" ^
      "\n@ 220:    :F:  $norm85 = $norm84($norm82);" ^
      "\n@ 305:    :F:  goto 304 if not $norm85;" ^
      "\n@ 224: 221:F:  $norm91 = y.__getattribute__;" ^
      "\n@ 226: 221:F:  $norm86 = $norm91;" ^
      "\n@ 228: 221:F:  $norm92 = \"__lt__\";" ^
      "\n@ 230: 221:F:  $norm94 = *get_call($norm86);" ^
      "\n@ 232: 221:F:  $norm93 = $norm94;" ^
      "\n@ 234: 221:F:  $norm95 = $norm93($norm92);" ^
      "\n@ 236: 221:F:  $norm89 = $norm95;" ^
      "\n@ 291:    :F:  goto 222;" ^
      "\n@ 221:    :F:  catch $norm90;" ^
      "\n@ 238:    :F:  $norm96 = builtin_type;" ^
      "\n@ 240:    :F:  $norm97 = $norm96($norm90);" ^
      "\n@ 242:    :F:  $norm98 = builtin_AttributeError;" ^
      "\n@ 244:    :F:  $norm99 = $norm97 is $norm98;" ^
      "\n@ 246:    :F:  $norm100 = builtin_bool;" ^
      "\n@ 248:    :F:  $norm101 = $norm100($norm99);" ^
      "\n@ 290:    :F:  goto 289 if not $norm101;" ^
      "\n@ 250:    :F:  $norm88 = $norm90;" ^
      "\n@ 254: 251:F:  $norm103 = y.__getattr__;" ^
      "\n@ 256: 251:F:  $norm87 = $norm103;" ^
      "\n@ 275:    :F:  goto 252;" ^
      "\n@ 251:    :F:  catch $norm102;" ^
      "\n@ 258:    :F:  $norm104 = builtin_type;" ^
      "\n@ 260:    :F:  $norm105 = $norm104($norm102);" ^
      "\n@ 262:    :F:  $norm106 = builtin_AttributeError;" ^
      "\n@ 264:    :F:  $norm107 = $norm105 is $norm106;" ^
      "\n@ 266:    :F:  $norm108 = builtin_bool;" ^
      "\n@ 268:    :F:  $norm109 = $norm108($norm107);" ^
      "\n@ 274:    :F:  goto 273 if not $norm109;" ^
      "\n@ 269:    :F:  raise $norm88;" ^
      "\n@ 272:    :F:  goto 271;" ^
      "\n@ 273:    :F:  pass;" ^
      "\n@ 270:    :F:  raise $norm102;" ^
      "\n@ 271:    :F:  pass;" ^
      "\n@ 252:    :F:  pass;" ^
      "\n@ 277:    :F:  $norm110 = \"__lt__\";" ^
      "\n@ 279:    :F:  $norm112 = *get_call($norm87);" ^
      "\n@ 281:    :F:  $norm111 = $norm112;" ^
      "\n@ 283:    :F:  $norm113 = $norm111($norm110);" ^
      "\n@ 285:    :F:  $norm89 = $norm113;" ^
      "\n@ 288:    :F:  goto 287;" ^
      "\n@ 289:    :F:  pass;" ^
      "\n@ 286:    :F:  raise $norm90;" ^
      "\n@ 287:    :F:  pass;" ^
      "\n@ 222:    :F:  pass;" ^
      "\n@ 293:    :F:  $norm115 = *get_call($norm89);" ^
      "\n@ 295:    :F:  $norm114 = $norm115;" ^
      "\n@ 297:    :F:  $norm116 = $norm114(z);" ^
      "\n@ 299:    :F:  $norm83 = $norm116;" ^
      "\n@ 303:    :F:  goto 302;" ^
      "\n@ 304:    :F:  pass;" ^
      "\n@ 301:    :F:  $norm83 = $norm82;" ^
      "\n@ 302:    :F:  pass;"
    end
;;

let operator_tests =
  [
    (* unop_plus_test;
       unop_minus_test;
       unop_not_test; *)
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
    "[1,0.0,'foo',x]"
    begin
      "@ 141:    :F:  $norm52 = 1;" ^
      "\n@ 143:    :F:  $norm53 = 0.000000;" ^
      "\n@ 145:    :F:  $norm54 = \"foo\";" ^
      "\n@ 147:    :F:  $norm55 = [$norm52, $norm53, $norm54, x];"
    end
;;

let tuple_test = gen_module_test "tuple_test"
    "(1,0.0,'foo',x)"
    begin
      "@ 141:    :F:  $norm52 = 1;" ^
      "\n@ 143:    :F:  $norm53 = 0.000000;" ^
      "\n@ 145:    :F:  $norm54 = \"foo\";" ^
      "\n@ 147:    :F:  $norm55 = ($norm52, $norm53, $norm54, x);"
    end

;;

let if_test = gen_module_test "if_test"
    "if x: z = 1\nelif y: z = -1\nelse: 0"
    begin
      "@ 141:    :F:  $norm52 = builtin_bool;" ^
      "\n@ 143:    :F:  $norm53 = $norm52(x);" ^
      "\n@ 169:    :F:  goto 168 if not $norm53;" ^
      "\n@ 145:    :F:  $norm54 = 1;" ^
      "\n@ 147:    :F:  $simp1 = $norm54;" ^
      "\n@ 149:    :F:  z = $simp1;" ^
      "\n@ 167:    :F:  goto 166;" ^
      "\n@ 168:    :F:  pass;" ^
      "\n@ 151:    :F:  $norm55 = builtin_bool;" ^
      "\n@ 153:    :F:  $norm56 = $norm55(y);" ^
      "\n@ 165:    :F:  goto 164 if not $norm56;" ^
      "\n@ 155:    :F:  $norm57 = -1;" ^
      "\n@ 157:    :F:  $simp0 = $norm57;" ^
      "\n@ 159:    :F:  z = $simp0;" ^
      "\n@ 163:    :F:  goto 162;" ^
      "\n@ 164:    :F:  pass;" ^
      "\n@ 161:    :F:  $norm58 = 0;" ^
      "\n@ 162:    :F:  pass;" ^
      "\n@ 166:    :F:  pass;"
    end
;;

let ifexp_test = gen_module_test "ifexp_test"
    "z = 1 if x else -1 if y else 0"
    begin
      "@ 141:    :F:  $norm53 = builtin_bool;" ^
      "\n@ 143:    :F:  $norm54 = $norm53(x);" ^
      "\n@ 169:    :F:  goto 168 if not $norm54;" ^
      "\n@ 145:    :F:  $norm55 = 1;" ^
      "\n@ 147:    :F:  $norm52 = $norm55;" ^
      "\n@ 167:    :F:  goto 166;" ^
      "\n@ 168:    :F:  pass;" ^
      "\n@ 149:    :F:  $norm57 = builtin_bool;" ^
      "\n@ 151:    :F:  $norm58 = $norm57(y);" ^
      "\n@ 163:    :F:  goto 162 if not $norm58;" ^
      "\n@ 153:    :F:  $norm59 = -1;" ^
      "\n@ 155:    :F:  $norm56 = $norm59;" ^
      "\n@ 161:    :F:  goto 160;" ^
      "\n@ 162:    :F:  pass;" ^
      "\n@ 157:    :F:  $norm60 = 0;" ^
      "\n@ 159:    :F:  $norm56 = $norm60;" ^
      "\n@ 160:    :F:  pass;" ^
      "\n@ 165:    :F:  $norm52 = $norm56;" ^
      "\n@ 166:    :F:  pass;" ^
      "\n@ 171:    :F:  $simp0 = $norm52;" ^
      "\n@ 173:    :F:  z = $simp0;"
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
      "@ 143: 140:F:  $norm53 = 5;" ^
      "\n@ 145: 140:F:  $simp0 = $norm53;" ^
      "\n@ 147: 140:F:  x = $simp0;" ^
      "\n@ 149: 140:F:  $norm55 = *get_call(somefunction);" ^
      "\n@ 151: 140:F:  $norm54 = $norm55;" ^
      "\n@ 153: 140:F:  $norm56 = $norm54();" ^
      "\n@ 189:    :F:  goto 141;" ^
      "\n@ 140:    :F:  catch $norm52;" ^
      "\n@ 155:    :F:  $norm57 = builtin_type;" ^
      "\n@ 157:    :F:  $norm58 = $norm57($norm52);" ^
      "\n@ 159:    :F:  $norm59 = $norm58 is ValueError;" ^
      "\n@ 161:    :F:  $norm60 = builtin_bool;" ^
      "\n@ 163:    :F:  $norm61 = $norm60($norm59);" ^
      "\n@ 188:    :F:  goto 187 if not $norm61;" ^
      "\n@ 165:    :F:  e = $norm52;" ^
      "\n@ 166:    :F:  pass;" ^
      "\n@ 186:    :F:  goto 185;" ^
      "\n@ 187:    :F:  pass;" ^
      "\n@ 168:    :F:  $norm62 = builtin_type;" ^
      "\n@ 170:    :F:  $norm63 = $norm62($norm52);" ^
      "\n@ 172:    :F:  $norm64 = $norm63 is Int;" ^
      "\n@ 174:    :F:  $norm65 = builtin_bool;" ^
      "\n@ 176:    :F:  $norm66 = $norm65($norm64);" ^
      "\n@ 184:    :F:  goto 183 if not $norm66;" ^
      "\n@ 178:    :F:  $norm67 = \"got an int\";" ^
      "\n@ 179:    :F:  print $norm67;" ^
      "\n@ 182:    :F:  goto 181;" ^
      "\n@ 183:    :F:  pass;" ^
      "\n@ 180:    :F:  raise $norm52;" ^
      "\n@ 181:    :F:  pass;" ^
      "\n@ 185:    :F:  pass;" ^
      "\n@ 141:    :F:  pass;"
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
      "@ 146:    :F:  $norm52 = def () {" ^
      "\n@ 141:    :F:    $simp0 = f;" ^
      "\n@ 143:    :F:    f$1_x = $simp0;" ^
      "\n@ 144:    :F:    return n;" ^
      "\n};" ^
      "\n@ 148:    :F:  f = $norm52;" ^
      "\n@ 150:    :F:  $norm54 = *get_call(f);" ^
      "\n@ 152:    :F:  $norm53 = $norm54;" ^
      "\n@ 154:    :F:  $norm55 = $norm53();" ^
      "\n@ 156:    :F:  $simp1 = $norm55;" ^
      "\n@ 158:    :F:  x = $simp1;"
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
      "@ 146:    :F:  $norm52 = def (f$1_x, f$1_y) {" ^
      "\n@ 141:    :F:    $simp0 = f;" ^
      "\n@ 143:    :F:    f$1_x = $simp0;" ^
      "\n@ 144:    :F:    return n;" ^
      "\n};" ^
      "\n@ 148:    :F:  f = $norm52;" ^
      "\n@ 150:    :F:  $norm54 = *get_call(f);" ^
      "\n@ 152:    :F:  $norm53 = $norm54;" ^
      "\n@ 154:    :F:  $norm55 = $norm53(z);" ^
      "\n@ 156:    :F:  $simp1 = $norm55;" ^
      "\n@ 158:    :F:  x = $simp1;"
    end
;;

let while_test = gen_module_test "while_test"
    "while True:\n\tx = x + 1"
    begin
      "@ 140:    :T:  pass;" ^
      "\n@ 143:    :T:  $norm52 = true;" ^
      "\n@ 145:    :T:  $norm53 = builtin_bool;" ^
      "\n@ 147:    :T:  $norm54 = $norm53($norm52);" ^
      "\n@ 231:    :T:  goto 141 if not $norm54;" ^
      "\n@ 151: 148:T:  $norm60 = x.__getattribute__;" ^
      "\n@ 153: 148:T:  $norm55 = $norm60;" ^
      "\n@ 155: 148:T:  $norm61 = \"__add__\";" ^
      "\n@ 157: 148:T:  $norm63 = *get_call($norm55);" ^
      "\n@ 159: 148:T:  $norm62 = $norm63;" ^
      "\n@ 161: 148:T:  $norm64 = $norm62($norm61);" ^
      "\n@ 163: 148:T:  $norm58 = $norm64;" ^
      "\n@ 218:    :T:  goto 149;" ^
      "\n@ 148:    :T:  catch $norm59;" ^
      "\n@ 165:    :T:  $norm65 = builtin_type;" ^
      "\n@ 167:    :T:  $norm66 = $norm65($norm59);" ^
      "\n@ 169:    :T:  $norm67 = builtin_AttributeError;" ^
      "\n@ 171:    :T:  $norm68 = $norm66 is $norm67;" ^
      "\n@ 173:    :T:  $norm69 = builtin_bool;" ^
      "\n@ 175:    :T:  $norm70 = $norm69($norm68);" ^
      "\n@ 217:    :T:  goto 216 if not $norm70;" ^
      "\n@ 177:    :T:  $norm57 = $norm59;" ^
      "\n@ 181: 178:T:  $norm72 = x.__getattr__;" ^
      "\n@ 183: 178:T:  $norm56 = $norm72;" ^
      "\n@ 202:    :T:  goto 179;" ^
      "\n@ 178:    :T:  catch $norm71;" ^
      "\n@ 185:    :T:  $norm73 = builtin_type;" ^
      "\n@ 187:    :T:  $norm74 = $norm73($norm71);" ^
      "\n@ 189:    :T:  $norm75 = builtin_AttributeError;" ^
      "\n@ 191:    :T:  $norm76 = $norm74 is $norm75;" ^
      "\n@ 193:    :T:  $norm77 = builtin_bool;" ^
      "\n@ 195:    :T:  $norm78 = $norm77($norm76);" ^
      "\n@ 201:    :T:  goto 200 if not $norm78;" ^
      "\n@ 196:    :T:  raise $norm57;" ^
      "\n@ 199:    :T:  goto 198;" ^
      "\n@ 200:    :T:  pass;" ^
      "\n@ 197:    :T:  raise $norm71;" ^
      "\n@ 198:    :T:  pass;" ^
      "\n@ 179:    :T:  pass;" ^
      "\n@ 204:    :T:  $norm79 = \"__add__\";" ^
      "\n@ 206:    :T:  $norm81 = *get_call($norm56);" ^
      "\n@ 208:    :T:  $norm80 = $norm81;" ^
      "\n@ 210:    :T:  $norm82 = $norm80($norm79);" ^
      "\n@ 212:    :T:  $norm58 = $norm82;" ^
      "\n@ 215:    :T:  goto 214;" ^
      "\n@ 216:    :T:  pass;" ^
      "\n@ 213:    :T:  raise $norm59;" ^
      "\n@ 214:    :T:  pass;" ^
      "\n@ 149:    :T:  pass;" ^
      "\n@ 220:    :T:  $norm83 = 1;" ^
      "\n@ 222:    :T:  $norm85 = *get_call($norm58);" ^
      "\n@ 224:    :T:  $norm84 = $norm85;" ^
      "\n@ 226:    :T:  $norm86 = $norm84($norm83);" ^
      "\n@ 228:    :T:  $simp0 = $norm86;" ^
      "\n@ 230:    :T:  x = $simp0;" ^
      "\n@ 232:    :T:  goto 140;" ^
      "\n@ 141:    :T:  pass;"
    end
;;

let break_test = gen_module_test "break_test"
    "while True:\n\tbreak"
    begin
      "@ 140:    :T:  pass;" ^
      "\n@ 143:    :T:  $norm52 = true;" ^
      "\n@ 145:    :T:  $norm53 = builtin_bool;" ^
      "\n@ 147:    :T:  $norm54 = $norm53($norm52);" ^
      "\n@ 149:    :T:  goto 141 if not $norm54;" ^
      "\n@ 148:    :T:  goto 141;" ^
      "\n@ 150:    :T:  goto 140;" ^
      "\n@ 141:    :T:  pass;"
    end
;;

let continue_test = gen_module_test "continue_test"
    "while True:\n\tcontinue"
    begin
      "@ 140:    :T:  pass;" ^
      "\n@ 143:    :T:  $norm52 = true;" ^
      "\n@ 145:    :T:  $norm53 = builtin_bool;" ^
      "\n@ 147:    :T:  $norm54 = $norm53($norm52);" ^
      "\n@ 149:    :T:  goto 141 if not $norm54;" ^
      "\n@ 148:    :T:  goto 140;" ^
      "\n@ 150:    :T:  goto 140;" ^
      "\n@ 141:    :T:  pass;"
    end
;;

let expect_error_test
    (name : string)
    (prog : string)
    (expected : exn) =
  name>::
  (fun _ ->
     assert_raises
       expected
       (fun _ ->
          parse_to_normalized prog true);
     Python2_ast_simplifier.reset_unique_name ();
     Python2_ast_normalizer.reset_unique_name ()
  )
;;

let bad_break_test = expect_error_test "bad_break_test"
    "break"
    (Failure("'break' outside loop"))
;;

let bad_continue_test = expect_error_test "bad_continue_test"
    "continue"
    (Failure("'continue' not properly in loop"))

;;

let expected_failures =
  [
    bad_break_test;
    bad_continue_test;
  ]
;;

let tests =
  "test_normalized_ast">:::
  literal_tests @
  stmt_tests @
  operator_tests @
  assignment_tests @
  [
    attribute_test;
    call_test;
    call_args_test;
    list_test;
    tuple_test;
    if_test;
    ifexp_test;
    tryexcept_test;
    funcdef_test;
    funcdef_args_test;
    while_test;
    break_test;
    continue_test;
  ] @
  expected_failures
