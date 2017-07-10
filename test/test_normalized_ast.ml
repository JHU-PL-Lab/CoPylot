open OUnit2;;
open Batteries;;
open Jhupllib;;
open Pp_utils;;

(* open Python2_ast_types;; *)
open Python2_ast_pipeline;;
(* open Python2_normalized_ast;; *)
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
(* let rec check_for_duplicates (lst : int list) : bool =
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

let rec collect_uids_stmt s =
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

let rec verify_unique_uids = function
  | Module(body, uid) ->
    let uids = List.concat (List.map collect_uids_stmt body) in
    check_for_duplicates (uid::uids)
;; *)

let gen_module_test (name : string) (prog : string)
    (expected : string) =
  name>::
  ( fun _ ->
      let actual = parse_to_normalized_safe prog 1 true in

      let pyssembly = pp_to_string pp_modl actual in
      let full_expected = expected in
      assert_equal ~printer:(fun x -> x) ~cmp:String.equal full_expected pyssembly
  )
;;

let gen_literal_test name prog exp =
  gen_module_test name prog
    ("@   2:    :F:  $norm0 = " ^ exp ^ ";")
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
    gen_module_test "return_var_test" "return x" "@   1:    :F:  return x;";

    gen_module_test "return_int_test" "return 5"
      "@   2:    :F:  $norm0 = 5;\n@   3:    :F:  return $norm0;";

    gen_module_test "return_none_test" "return"
      "@   1:    :F:  return *None;";


    gen_module_test "print_test1" "print x" "@   1:    :F:  print x;";
    gen_module_test "print_test2" "print x,y" "@   1:    :F:  print x, y;";

    gen_module_test "raise_test" "raise x" "@   1:    :F:  raise x;";

    gen_module_test "pass_test" "pass" "@   1:    :F:  pass;";
  ]
;;

let attribute_test = gen_module_test "attribute_test"
    "x.mem"
    begin
      "@   4:   1:F:  $norm5 = \"__getattribute__\";" ^
      "\n@   6:   1:F:  $norm6 = *get_attribute(x, $norm5);" ^
      "\n@   8:   1:F:  $norm0 = $norm6;" ^
      "\n@  10:   1:F:  $norm7 = \"mem\";" ^
      "\n@  12:   1:F:  $norm9 = *get_call($norm0);" ^
      "\n@  14:   1:F:  $norm8 = $norm9;" ^
      "\n@  16:   1:F:  $norm10 = $norm8($norm7);" ^
      "\n@  18:   1:F:  $norm3 = $norm10;" ^
      "\n@  75:    :F:  goto 2;" ^
      "\n@   1:    :F:  catch $norm4;" ^
      "\n@  20:    :F:  $norm11 = *type;" ^
      "\n@  22:    :F:  $norm12 = $norm11($norm4);" ^
      "\n@  24:    :F:  $norm13 = *AttributeError;" ^
      "\n@  26:    :F:  $norm14 = $norm12 is $norm13;" ^
      "\n@  28:    :F:  $norm15 = *bool;" ^
      "\n@  30:    :F:  $norm16 = $norm15($norm14);" ^
      "\n@  74:    :F:  goto 73 if not $norm16;" ^
      "\n@  32:    :F:  $norm2 = $norm4;" ^
      "\n@  36:  33:F:  $norm18 = \"__getattr__\";" ^
      "\n@  38:  33:F:  $norm19 = *get_attribute(x, $norm18);" ^
      "\n@  40:  33:F:  $norm1 = $norm19;" ^
      "\n@  59:    :F:  goto 34;" ^
      "\n@  33:    :F:  catch $norm17;" ^
      "\n@  42:    :F:  $norm20 = *type;" ^
      "\n@  44:    :F:  $norm21 = $norm20($norm17);" ^
      "\n@  46:    :F:  $norm22 = *AttributeError;" ^
      "\n@  48:    :F:  $norm23 = $norm21 is $norm22;" ^
      "\n@  50:    :F:  $norm24 = *bool;" ^
      "\n@  52:    :F:  $norm25 = $norm24($norm23);" ^
      "\n@  58:    :F:  goto 57 if not $norm25;" ^
      "\n@  53:    :F:  raise $norm2;" ^
      "\n@  56:    :F:  goto 55;" ^
      "\n@  57:    :F:  pass;" ^
      "\n@  54:    :F:  raise $norm17;" ^
      "\n@  55:    :F:  pass;" ^
      "\n@  34:    :F:  pass;" ^
      "\n@  61:    :F:  $norm26 = \"mem\";" ^
      "\n@  63:    :F:  $norm28 = *get_call($norm1);" ^
      "\n@  65:    :F:  $norm27 = $norm28;" ^
      "\n@  67:    :F:  $norm29 = $norm27($norm26);" ^
      "\n@  69:    :F:  $norm3 = $norm29;" ^
      "\n@  72:    :F:  goto 71;" ^
      "\n@  73:    :F:  pass;" ^
      "\n@  70:    :F:  raise $norm4;" ^
      "\n@  71:    :F:  pass;" ^
      "\n@   2:    :F:  pass;"
    end
;;

let call_test = gen_module_test "call_test"
    "foo()"
    begin
      "@   2:    :F:  $norm1 = *get_call(foo);" ^
      "\n@   4:    :F:  $norm0 = $norm1;" ^
      "\n@   6:    :F:  $norm2 = $norm0();"
    end
;;

let call_args_test = gen_module_test "call_args_test"
    "foo(1,x)"
    begin
      "@   2:    :F:  $norm0 = 1;" ^
      "\n@   4:    :F:  $norm2 = *get_call(foo);" ^
      "\n@   6:    :F:  $norm1 = $norm2;" ^
      "\n@   8:    :F:  $norm3 = $norm1($norm0, x);"
    end

;;

let assign_test = gen_module_test "assign_test"
    "x = 5"
    begin
      "@   2:    :F:  $norm0 = 5;" ^
      "\n@   4:    :F:  $simp0 = $norm0;" ^
      "\n@   6:    :F:  x = $simp0;"
    end
;;

let augassign_test = gen_module_test "augassign_test"
    "x += 5"
    begin
      "@   6:   3:F:  $norm6 = \"__getattribute__\";" ^
      "\n@   8:   3:F:  $norm7 = *get_attribute(x, $norm6);" ^
      "\n@  10:   3:F:  $norm1 = $norm7;" ^
      "\n@  12:   3:F:  $norm8 = \"__iadd__\";" ^
      "\n@  14:   3:F:  $norm10 = *get_call($norm1);" ^
      "\n@  16:   3:F:  $norm9 = $norm10;" ^
      "\n@  18:   3:F:  $norm11 = $norm9($norm8);" ^
      "\n@  20:   3:F:  $norm4 = $norm11;" ^
      "\n@  77:   1:F:  goto 4;" ^
      "\n@   3:   1:F:  catch $norm5;" ^
      "\n@  22:   1:F:  $norm12 = *type;" ^
      "\n@  24:   1:F:  $norm13 = $norm12($norm5);" ^
      "\n@  26:   1:F:  $norm14 = *AttributeError;" ^
      "\n@  28:   1:F:  $norm15 = $norm13 is $norm14;" ^
      "\n@  30:   1:F:  $norm16 = *bool;" ^
      "\n@  32:   1:F:  $norm17 = $norm16($norm15);" ^
      "\n@  76:   1:F:  goto 75 if not $norm17;" ^
      "\n@  34:   1:F:  $norm3 = $norm5;" ^
      "\n@  38:  35:F:  $norm19 = \"__getattr__\";" ^
      "\n@  40:  35:F:  $norm20 = *get_attribute(x, $norm19);" ^
      "\n@  42:  35:F:  $norm2 = $norm20;" ^
      "\n@  61:   1:F:  goto 36;" ^
      "\n@  35:   1:F:  catch $norm18;" ^
      "\n@  44:   1:F:  $norm21 = *type;" ^
      "\n@  46:   1:F:  $norm22 = $norm21($norm18);" ^
      "\n@  48:   1:F:  $norm23 = *AttributeError;" ^
      "\n@  50:   1:F:  $norm24 = $norm22 is $norm23;" ^
      "\n@  52:   1:F:  $norm25 = *bool;" ^
      "\n@  54:   1:F:  $norm26 = $norm25($norm24);" ^
      "\n@  60:   1:F:  goto 59 if not $norm26;" ^
      "\n@  55:   1:F:  raise $norm3;" ^
      "\n@  58:   1:F:  goto 57;" ^
      "\n@  59:   1:F:  pass;" ^
      "\n@  56:   1:F:  raise $norm18;" ^
      "\n@  57:   1:F:  pass;" ^
      "\n@  36:   1:F:  pass;" ^
      "\n@  63:   1:F:  $norm27 = \"__iadd__\";" ^
      "\n@  65:   1:F:  $norm29 = *get_call($norm2);" ^
      "\n@  67:   1:F:  $norm28 = $norm29;" ^
      "\n@  69:   1:F:  $norm30 = $norm28($norm27);" ^
      "\n@  71:   1:F:  $norm4 = $norm30;" ^
      "\n@  74:   1:F:  goto 73;" ^
      "\n@  75:   1:F:  pass;" ^
      "\n@  72:   1:F:  raise $norm5;" ^
      "\n@  73:   1:F:  pass;" ^
      "\n@   4:   1:F:  pass;" ^
      "\n@  79:   1:F:  $simp4 = $norm4;" ^
      "\n@  81:   1:F:  $simp1 = $simp4;" ^
      "\n@ 178:    :F:  goto 2;" ^
      "\n@   1:    :F:  catch $norm0;" ^
      "\n@  83:    :F:  $norm31 = *type;" ^
      "\n@  85:    :F:  $norm32 = $norm31($norm0);" ^
      "\n@  87:    :F:  $norm33 = *AttributeError;" ^
      "\n@  89:    :F:  $norm34 = $norm32 is $norm33;" ^
      "\n@  91:    :F:  $norm35 = *bool;" ^
      "\n@  93:    :F:  $norm36 = $norm35($norm34);" ^
      "\n@ 177:    :F:  goto 176 if not $norm36;" ^
      "\n@  97:  94:F:  $norm42 = \"__getattribute__\";" ^
      "\n@  99:  94:F:  $norm43 = *get_attribute(x, $norm42);" ^
      "\n@ 101:  94:F:  $norm37 = $norm43;" ^
      "\n@ 103:  94:F:  $norm44 = \"__add__\";" ^
      "\n@ 105:  94:F:  $norm46 = *get_call($norm37);" ^
      "\n@ 107:  94:F:  $norm45 = $norm46;" ^
      "\n@ 109:  94:F:  $norm47 = $norm45($norm44);" ^
      "\n@ 111:  94:F:  $norm40 = $norm47;" ^
      "\n@ 168:    :F:  goto 95;" ^
      "\n@  94:    :F:  catch $norm41;" ^
      "\n@ 113:    :F:  $norm48 = *type;" ^
      "\n@ 115:    :F:  $norm49 = $norm48($norm41);" ^
      "\n@ 117:    :F:  $norm50 = *AttributeError;" ^
      "\n@ 119:    :F:  $norm51 = $norm49 is $norm50;" ^
      "\n@ 121:    :F:  $norm52 = *bool;" ^
      "\n@ 123:    :F:  $norm53 = $norm52($norm51);" ^
      "\n@ 167:    :F:  goto 166 if not $norm53;" ^
      "\n@ 125:    :F:  $norm39 = $norm41;" ^
      "\n@ 129: 126:F:  $norm55 = \"__getattr__\";" ^
      "\n@ 131: 126:F:  $norm56 = *get_attribute(x, $norm55);" ^
      "\n@ 133: 126:F:  $norm38 = $norm56;" ^
      "\n@ 152:    :F:  goto 127;" ^
      "\n@ 126:    :F:  catch $norm54;" ^
      "\n@ 135:    :F:  $norm57 = *type;" ^
      "\n@ 137:    :F:  $norm58 = $norm57($norm54);" ^
      "\n@ 139:    :F:  $norm59 = *AttributeError;" ^
      "\n@ 141:    :F:  $norm60 = $norm58 is $norm59;" ^
      "\n@ 143:    :F:  $norm61 = *bool;" ^
      "\n@ 145:    :F:  $norm62 = $norm61($norm60);" ^
      "\n@ 151:    :F:  goto 150 if not $norm62;" ^
      "\n@ 146:    :F:  raise $norm39;" ^
      "\n@ 149:    :F:  goto 148;" ^
      "\n@ 150:    :F:  pass;" ^
      "\n@ 147:    :F:  raise $norm54;" ^
      "\n@ 148:    :F:  pass;" ^
      "\n@ 127:    :F:  pass;" ^
      "\n@ 154:    :F:  $norm63 = \"__add__\";" ^
      "\n@ 156:    :F:  $norm65 = *get_call($norm38);" ^
      "\n@ 158:    :F:  $norm64 = $norm65;" ^
      "\n@ 160:    :F:  $norm66 = $norm64($norm63);" ^
      "\n@ 162:    :F:  $norm40 = $norm66;" ^
      "\n@ 165:    :F:  goto 164;" ^
      "\n@ 166:    :F:  pass;" ^
      "\n@ 163:    :F:  raise $norm41;" ^
      "\n@ 164:    :F:  pass;" ^
      "\n@  95:    :F:  pass;" ^
      "\n@ 170:    :F:  $simp3 = $norm40;" ^
      "\n@ 172:    :F:  $simp1 = $simp3;" ^
      "\n@ 175:    :F:  goto 174;" ^
      "\n@ 176:    :F:  pass;" ^
      "\n@ 173:    :F:  raise $norm0;" ^
      "\n@ 174:    :F:  pass;" ^
      "\n@   2:    :F:  pass;" ^
      "\n@ 180:    :F:  $norm67 = 5;" ^
      "\n@ 182:    :F:  $norm69 = *get_call($simp1);" ^
      "\n@ 184:    :F:  $norm68 = $norm69;" ^
      "\n@ 186:    :F:  $norm70 = $norm68($norm67);" ^
      "\n@ 188:    :F:  $simp2 = $norm70;" ^
      "\n@ 190:    :F:  x = $simp2;"
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
      "\n@   4:    :F:  $norm0 = *slice;" ^
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
    begin
      "@   2:    :F:  $norm0 = 5;" ^
      "\n@   4:    :F:  $simp0 = $norm0;" ^
      "\n@   8:   5:F:  $norm6 = *get_attribute(x, __getattribute__);" ^
      "\n@  10:   5:F:  $norm1 = $norm6;" ^
      "\n@  12:   5:F:  $norm7 = \"__setattr__\";" ^
      "\n@  14:   5:F:  $norm9 = *get_call($norm1);" ^
      "\n@  16:   5:F:  $norm8 = $norm9;" ^
      "\n@  18:   5:F:  $norm10 = $norm8($norm7);" ^
      "\n@  20:   5:F:  $norm4 = $norm10;" ^
      "\n@  75:    :F:  goto 6;" ^
      "\n@   5:    :F:  catch $norm5;" ^
      "\n@  22:    :F:  $norm11 = *type;" ^
      "\n@  24:    :F:  $norm12 = $norm11($norm5);" ^
      "\n@  26:    :F:  $norm13 = *AttributeError;" ^
      "\n@  28:    :F:  $norm14 = $norm12 is $norm13;" ^
      "\n@  30:    :F:  $norm15 = *bool;" ^
      "\n@  32:    :F:  $norm16 = $norm15($norm14);" ^
      "\n@  74:    :F:  goto 73 if not $norm16;" ^
      "\n@  34:    :F:  $norm3 = $norm5;" ^
      "\n@  38:  35:F:  $norm18 = *get_attribute(x, __getattr__);" ^
      "\n@  40:  35:F:  $norm2 = $norm18;" ^
      "\n@  59:    :F:  goto 36;" ^
      "\n@  35:    :F:  catch $norm17;" ^
      "\n@  42:    :F:  $norm19 = *type;" ^
      "\n@  44:    :F:  $norm20 = $norm19($norm17);" ^
      "\n@  46:    :F:  $norm21 = *AttributeError;" ^
      "\n@  48:    :F:  $norm22 = $norm20 is $norm21;" ^
      "\n@  50:    :F:  $norm23 = *bool;" ^
      "\n@  52:    :F:  $norm24 = $norm23($norm22);" ^
      "\n@  58:    :F:  goto 57 if not $norm24;" ^
      "\n@  53:    :F:  raise $norm3;" ^
      "\n@  56:    :F:  goto 55;" ^
      "\n@  57:    :F:  pass;" ^
      "\n@  54:    :F:  raise $norm17;" ^
      "\n@  55:    :F:  pass;" ^
      "\n@  36:    :F:  pass;" ^
      "\n@  61:    :F:  $norm25 = \"__setattr__\";" ^
      "\n@  63:    :F:  $norm27 = *get_call($norm2);" ^
      "\n@  65:    :F:  $norm26 = $norm27;" ^
      "\n@  67:    :F:  $norm28 = $norm26($norm25);" ^
      "\n@  69:    :F:  $norm4 = $norm28;" ^
      "\n@  72:    :F:  goto 71;" ^
      "\n@  73:    :F:  pass;" ^
      "\n@  70:    :F:  raise $norm5;" ^
      "\n@  71:    :F:  pass;" ^
      "\n@   6:    :F:  pass;" ^
      "\n@  77:    :F:  $norm29 = \"mem\";" ^
      "\n@  79:    :F:  $norm31 = *get_call($norm4);" ^
      "\n@  81:    :F:  $norm30 = $norm31;" ^
      "\n@  83:    :F:  $norm32 = $norm30($norm29, $simp0);"
    end
;;

let assign_to_index_test = gen_module_test "assign_to_index_test"
    "list[5] = 5"
    ("@   2:    :F:  $norm0 = 5;" ^
     "\n@   4:    :F:  $simp0 = $norm0;" ^
     "\n@   6:    :F:  $norm1 = *get_attribute(list, __setitem__);" ^
     "\n@   8:    :F:  $norm2 = 5;" ^
     "\n@  10:    :F:  $norm3 = $norm1($norm2, $simp0);")
;;

let assign_to_slice_test = gen_module_test "assign_to_slice_test"
    "list[1:2] = 5"
    ("@   2:    :F:  $norm0 = 5;" ^
     "\n@   4:    :F:  $simp0 = $norm0;" ^
     "\n@   6:    :F:  $norm1 = *get_attribute(list, __setitem__);" ^
     "\n@   8:    :F:  $norm2 = *slice;" ^
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
    (* augassign_to_member_test;
       augassign_to_list_test;
       multiassign_test;
       assign_to_member_test;
       assign_to_index_test;
       assign_to_slice_test;
       assign_from_tuple_test; *)
  ]
;;

let unop_plus_test = gen_module_test "unop_plus_test"
    "+x"
    begin
      "@   4:   1:F:  $norm5 = \"__getattribute__\";" ^
      "\n@   6:   1:F:  $norm6 = *get_attribute(x, $norm5);" ^
      "\n@   8:   1:F:  $norm0 = $norm6;" ^
      "\n@  10:   1:F:  $norm7 = \"__pos__\";" ^
      "\n@  12:   1:F:  $norm9 = *get_call($norm0);" ^
      "\n@  14:   1:F:  $norm8 = $norm9;" ^
      "\n@  16:   1:F:  $norm10 = $norm8($norm7);" ^
      "\n@  18:   1:F:  $norm3 = $norm10;" ^
      "\n@  75:    :F:  goto 2;" ^
      "\n@   1:    :F:  catch $norm4;" ^
      "\n@  20:    :F:  $norm11 = *type;" ^
      "\n@  22:    :F:  $norm12 = $norm11($norm4);" ^
      "\n@  24:    :F:  $norm13 = *AttributeError;" ^
      "\n@  26:    :F:  $norm14 = $norm12 is $norm13;" ^
      "\n@  28:    :F:  $norm15 = *bool;" ^
      "\n@  30:    :F:  $norm16 = $norm15($norm14);" ^
      "\n@  74:    :F:  goto 73 if not $norm16;" ^
      "\n@  32:    :F:  $norm2 = $norm4;" ^
      "\n@  36:  33:F:  $norm18 = \"__getattr__\";" ^
      "\n@  38:  33:F:  $norm19 = *get_attribute(x, $norm18);" ^
      "\n@  40:  33:F:  $norm1 = $norm19;" ^
      "\n@  59:    :F:  goto 34;" ^
      "\n@  33:    :F:  catch $norm17;" ^
      "\n@  42:    :F:  $norm20 = *type;" ^
      "\n@  44:    :F:  $norm21 = $norm20($norm17);" ^
      "\n@  46:    :F:  $norm22 = *AttributeError;" ^
      "\n@  48:    :F:  $norm23 = $norm21 is $norm22;" ^
      "\n@  50:    :F:  $norm24 = *bool;" ^
      "\n@  52:    :F:  $norm25 = $norm24($norm23);" ^
      "\n@  58:    :F:  goto 57 if not $norm25;" ^
      "\n@  53:    :F:  raise $norm2;" ^
      "\n@  56:    :F:  goto 55;" ^
      "\n@  57:    :F:  pass;" ^
      "\n@  54:    :F:  raise $norm17;" ^
      "\n@  55:    :F:  pass;" ^
      "\n@  34:    :F:  pass;" ^
      "\n@  61:    :F:  $norm26 = \"__pos__\";" ^
      "\n@  63:    :F:  $norm28 = *get_call($norm1);" ^
      "\n@  65:    :F:  $norm27 = $norm28;" ^
      "\n@  67:    :F:  $norm29 = $norm27($norm26);" ^
      "\n@  69:    :F:  $norm3 = $norm29;" ^
      "\n@  72:    :F:  goto 71;" ^
      "\n@  73:    :F:  pass;" ^
      "\n@  70:    :F:  raise $norm4;" ^
      "\n@  71:    :F:  pass;" ^
      "\n@   2:    :F:  pass;" ^
      "\n@  77:    :F:  $norm31 = *get_call($norm3);" ^
      "\n@  79:    :F:  $norm30 = $norm31;" ^
      "\n@  81:    :F:  $norm32 = $norm30();"
    end
;;

let unop_minus_test = gen_module_test "unop_minus_test"
    "-x"
    begin
      "@   4:   1:F:  $norm5 = \"__getattribute__\";" ^
      "\n@   6:   1:F:  $norm6 = *get_attribute(x, $norm5);" ^
      "\n@   8:   1:F:  $norm0 = $norm6;" ^
      "\n@  10:   1:F:  $norm7 = \"__neg__\";" ^
      "\n@  12:   1:F:  $norm9 = *get_call($norm0);" ^
      "\n@  14:   1:F:  $norm8 = $norm9;" ^
      "\n@  16:   1:F:  $norm10 = $norm8($norm7);" ^
      "\n@  18:   1:F:  $norm3 = $norm10;" ^
      "\n@  75:    :F:  goto 2;" ^
      "\n@   1:    :F:  catch $norm4;" ^
      "\n@  20:    :F:  $norm11 = *type;" ^
      "\n@  22:    :F:  $norm12 = $norm11($norm4);" ^
      "\n@  24:    :F:  $norm13 = *AttributeError;" ^
      "\n@  26:    :F:  $norm14 = $norm12 is $norm13;" ^
      "\n@  28:    :F:  $norm15 = *bool;" ^
      "\n@  30:    :F:  $norm16 = $norm15($norm14);" ^
      "\n@  74:    :F:  goto 73 if not $norm16;" ^
      "\n@  32:    :F:  $norm2 = $norm4;" ^
      "\n@  36:  33:F:  $norm18 = \"__getattr__\";" ^
      "\n@  38:  33:F:  $norm19 = *get_attribute(x, $norm18);" ^
      "\n@  40:  33:F:  $norm1 = $norm19;" ^
      "\n@  59:    :F:  goto 34;" ^
      "\n@  33:    :F:  catch $norm17;" ^
      "\n@  42:    :F:  $norm20 = *type;" ^
      "\n@  44:    :F:  $norm21 = $norm20($norm17);" ^
      "\n@  46:    :F:  $norm22 = *AttributeError;" ^
      "\n@  48:    :F:  $norm23 = $norm21 is $norm22;" ^
      "\n@  50:    :F:  $norm24 = *bool;" ^
      "\n@  52:    :F:  $norm25 = $norm24($norm23);" ^
      "\n@  58:    :F:  goto 57 if not $norm25;" ^
      "\n@  53:    :F:  raise $norm2;" ^
      "\n@  56:    :F:  goto 55;" ^
      "\n@  57:    :F:  pass;" ^
      "\n@  54:    :F:  raise $norm17;" ^
      "\n@  55:    :F:  pass;" ^
      "\n@  34:    :F:  pass;" ^
      "\n@  61:    :F:  $norm26 = \"__neg__\";" ^
      "\n@  63:    :F:  $norm28 = *get_call($norm1);" ^
      "\n@  65:    :F:  $norm27 = $norm28;" ^
      "\n@  67:    :F:  $norm29 = $norm27($norm26);" ^
      "\n@  69:    :F:  $norm3 = $norm29;" ^
      "\n@  72:    :F:  goto 71;" ^
      "\n@  73:    :F:  pass;" ^
      "\n@  70:    :F:  raise $norm4;" ^
      "\n@  71:    :F:  pass;" ^
      "\n@   2:    :F:  pass;" ^
      "\n@  77:    :F:  $norm31 = *get_call($norm3);" ^
      "\n@  79:    :F:  $norm30 = $norm31;" ^
      "\n@  81:    :F:  $norm32 = $norm30();"
    end
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
  gen_module_test name ("x " ^ opstring ^ " y")
    begin
      "@   4:   1:F:  $norm5 = \"__getattribute__\";" ^
      "\n@   6:   1:F:  $norm6 = *get_attribute(x, $norm5);" ^
      "\n@   8:   1:F:  $norm0 = $norm6;" ^
      "\n@  10:   1:F:  $norm7 = \"" ^ opfunc ^ "\";" ^
      "\n@  12:   1:F:  $norm9 = *get_call($norm0);" ^
      "\n@  14:   1:F:  $norm8 = $norm9;" ^
      "\n@  16:   1:F:  $norm10 = $norm8($norm7);" ^
      "\n@  18:   1:F:  $norm3 = $norm10;" ^
      "\n@  75:    :F:  goto 2;" ^
      "\n@   1:    :F:  catch $norm4;" ^
      "\n@  20:    :F:  $norm11 = *type;" ^
      "\n@  22:    :F:  $norm12 = $norm11($norm4);" ^
      "\n@  24:    :F:  $norm13 = *AttributeError;" ^
      "\n@  26:    :F:  $norm14 = $norm12 is $norm13;" ^
      "\n@  28:    :F:  $norm15 = *bool;" ^
      "\n@  30:    :F:  $norm16 = $norm15($norm14);" ^
      "\n@  74:    :F:  goto 73 if not $norm16;" ^
      "\n@  32:    :F:  $norm2 = $norm4;" ^
      "\n@  36:  33:F:  $norm18 = \"__getattr__\";" ^
      "\n@  38:  33:F:  $norm19 = *get_attribute(x, $norm18);" ^
      "\n@  40:  33:F:  $norm1 = $norm19;" ^
      "\n@  59:    :F:  goto 34;" ^
      "\n@  33:    :F:  catch $norm17;" ^
      "\n@  42:    :F:  $norm20 = *type;" ^
      "\n@  44:    :F:  $norm21 = $norm20($norm17);" ^
      "\n@  46:    :F:  $norm22 = *AttributeError;" ^
      "\n@  48:    :F:  $norm23 = $norm21 is $norm22;" ^
      "\n@  50:    :F:  $norm24 = *bool;" ^
      "\n@  52:    :F:  $norm25 = $norm24($norm23);" ^
      "\n@  58:    :F:  goto 57 if not $norm25;" ^
      "\n@  53:    :F:  raise $norm2;" ^
      "\n@  56:    :F:  goto 55;" ^
      "\n@  57:    :F:  pass;" ^
      "\n@  54:    :F:  raise $norm17;" ^
      "\n@  55:    :F:  pass;" ^
      "\n@  34:    :F:  pass;" ^
      "\n@  61:    :F:  $norm26 = \"" ^ opfunc ^ "\";" ^
      "\n@  63:    :F:  $norm28 = *get_call($norm1);" ^
      "\n@  65:    :F:  $norm27 = $norm28;" ^
      "\n@  67:    :F:  $norm29 = $norm27($norm26);" ^
      "\n@  69:    :F:  $norm3 = $norm29;" ^
      "\n@  72:    :F:  goto 71;" ^
      "\n@  73:    :F:  pass;" ^
      "\n@  70:    :F:  raise $norm4;" ^
      "\n@  71:    :F:  pass;" ^
      "\n@   2:    :F:  pass;" ^
      "\n@  77:    :F:  $norm31 = *get_call($norm3);" ^
      "\n@  79:    :F:  $norm30 = $norm31;" ^
      "\n@  81:    :F:  $norm32 = $norm30(y);"
    end
;;

let boolop_and_test = gen_module_test "boolop_and_test"
    "x and 5 and True"
    begin
      "@   2:    :F:  $norm1 = *bool;" ^
      "\n@   4:    :F:  $norm2 = $norm1(x);" ^
      "\n@  28:    :F:  goto 27 if not $norm2;" ^
      "\n@   6:    :F:  $norm3 = 5;" ^
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
      "\n@  25:    :F:  pass;"
    end
;;

let boolop_or_test = gen_module_test "boolop_or_test"
    "x or 5 or True"
    begin
      "@   2:    :F:  $norm1 = *bool;" ^
      "\n@   4:    :F:  $norm2 = $norm1(x);" ^
      "\n@  28:    :F:  goto 27 if not $norm2;" ^
      "\n@   6:    :F:  $norm0 = x;" ^
      "\n@  26:    :F:  goto 25;" ^
      "\n@  27:    :F:  pass;" ^
      "\n@   8:    :F:  $norm3 = 5;" ^
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
      "\n@  25:    :F:  pass;"
    end
;;

let gen_cmpop_test (name : string) (opstring : string) (opfunc : string ) =
  (* gen_module_test name ("x " ^ opstring ^ " y")
     begin
      "@   2:    :F:  $norm0 = *get_attribute(x, " ^ opfunc ^ ");" ^
      "\n@   4:    :F:  $norm1 = $norm0(y);"
     end *)
  gen_binop_test name opstring opfunc
;;

let multi_compare_test = gen_module_test "multi_compare_test"
    "x < y < z"
    begin
      "@   4:   1:F:  $norm5 = \"__getattribute__\";" ^
      "\n@   6:   1:F:  $norm6 = *get_attribute(x, $norm5);" ^
      "\n@   8:   1:F:  $norm0 = $norm6;" ^
      "\n@  10:   1:F:  $norm7 = \"__lt__\";" ^
      "\n@  12:   1:F:  $norm9 = *get_call($norm0);" ^
      "\n@  14:   1:F:  $norm8 = $norm9;" ^
      "\n@  16:   1:F:  $norm10 = $norm8($norm7);" ^
      "\n@  18:   1:F:  $norm3 = $norm10;" ^
      "\n@  75:    :F:  goto 2;" ^
      "\n@   1:    :F:  catch $norm4;" ^
      "\n@  20:    :F:  $norm11 = *type;" ^
      "\n@  22:    :F:  $norm12 = $norm11($norm4);" ^
      "\n@  24:    :F:  $norm13 = *AttributeError;" ^
      "\n@  26:    :F:  $norm14 = $norm12 is $norm13;" ^
      "\n@  28:    :F:  $norm15 = *bool;" ^
      "\n@  30:    :F:  $norm16 = $norm15($norm14);" ^
      "\n@  74:    :F:  goto 73 if not $norm16;" ^
      "\n@  32:    :F:  $norm2 = $norm4;" ^
      "\n@  36:  33:F:  $norm18 = \"__getattr__\";" ^
      "\n@  38:  33:F:  $norm19 = *get_attribute(x, $norm18);" ^
      "\n@  40:  33:F:  $norm1 = $norm19;" ^
      "\n@  59:    :F:  goto 34;" ^
      "\n@  33:    :F:  catch $norm17;" ^
      "\n@  42:    :F:  $norm20 = *type;" ^
      "\n@  44:    :F:  $norm21 = $norm20($norm17);" ^
      "\n@  46:    :F:  $norm22 = *AttributeError;" ^
      "\n@  48:    :F:  $norm23 = $norm21 is $norm22;" ^
      "\n@  50:    :F:  $norm24 = *bool;" ^
      "\n@  52:    :F:  $norm25 = $norm24($norm23);" ^
      "\n@  58:    :F:  goto 57 if not $norm25;" ^
      "\n@  53:    :F:  raise $norm2;" ^
      "\n@  56:    :F:  goto 55;" ^
      "\n@  57:    :F:  pass;" ^
      "\n@  54:    :F:  raise $norm17;" ^
      "\n@  55:    :F:  pass;" ^
      "\n@  34:    :F:  pass;" ^
      "\n@  61:    :F:  $norm26 = \"__lt__\";" ^
      "\n@  63:    :F:  $norm28 = *get_call($norm1);" ^
      "\n@  65:    :F:  $norm27 = $norm28;" ^
      "\n@  67:    :F:  $norm29 = $norm27($norm26);" ^
      "\n@  69:    :F:  $norm3 = $norm29;" ^
      "\n@  72:    :F:  goto 71;" ^
      "\n@  73:    :F:  pass;" ^
      "\n@  70:    :F:  raise $norm4;" ^
      "\n@  71:    :F:  pass;" ^
      "\n@   2:    :F:  pass;" ^
      "\n@  77:    :F:  $norm31 = *get_call($norm3);" ^
      "\n@  79:    :F:  $norm30 = $norm31;" ^
      "\n@  81:    :F:  $norm32 = $norm30(y);" ^
      "\n@  83:    :F:  $norm34 = *bool;" ^
      "\n@  85:    :F:  $norm35 = $norm34($norm32);" ^
      "\n@ 174:    :F:  goto 173 if not $norm35;" ^
      "\n@  89:  86:F:  $norm41 = \"__getattribute__\";" ^
      "\n@  91:  86:F:  $norm42 = *get_attribute(y, $norm41);" ^
      "\n@  93:  86:F:  $norm36 = $norm42;" ^
      "\n@  95:  86:F:  $norm43 = \"__lt__\";" ^
      "\n@  97:  86:F:  $norm45 = *get_call($norm36);" ^
      "\n@  99:  86:F:  $norm44 = $norm45;" ^
      "\n@ 101:  86:F:  $norm46 = $norm44($norm43);" ^
      "\n@ 103:  86:F:  $norm39 = $norm46;" ^
      "\n@ 160:    :F:  goto 87;" ^
      "\n@  86:    :F:  catch $norm40;" ^
      "\n@ 105:    :F:  $norm47 = *type;" ^
      "\n@ 107:    :F:  $norm48 = $norm47($norm40);" ^
      "\n@ 109:    :F:  $norm49 = *AttributeError;" ^
      "\n@ 111:    :F:  $norm50 = $norm48 is $norm49;" ^
      "\n@ 113:    :F:  $norm51 = *bool;" ^
      "\n@ 115:    :F:  $norm52 = $norm51($norm50);" ^
      "\n@ 159:    :F:  goto 158 if not $norm52;" ^
      "\n@ 117:    :F:  $norm38 = $norm40;" ^
      "\n@ 121: 118:F:  $norm54 = \"__getattr__\";" ^
      "\n@ 123: 118:F:  $norm55 = *get_attribute(y, $norm54);" ^
      "\n@ 125: 118:F:  $norm37 = $norm55;" ^
      "\n@ 144:    :F:  goto 119;" ^
      "\n@ 118:    :F:  catch $norm53;" ^
      "\n@ 127:    :F:  $norm56 = *type;" ^
      "\n@ 129:    :F:  $norm57 = $norm56($norm53);" ^
      "\n@ 131:    :F:  $norm58 = *AttributeError;" ^
      "\n@ 133:    :F:  $norm59 = $norm57 is $norm58;" ^
      "\n@ 135:    :F:  $norm60 = *bool;" ^
      "\n@ 137:    :F:  $norm61 = $norm60($norm59);" ^
      "\n@ 143:    :F:  goto 142 if not $norm61;" ^
      "\n@ 138:    :F:  raise $norm38;" ^
      "\n@ 141:    :F:  goto 140;" ^
      "\n@ 142:    :F:  pass;" ^
      "\n@ 139:    :F:  raise $norm53;" ^
      "\n@ 140:    :F:  pass;" ^
      "\n@ 119:    :F:  pass;" ^
      "\n@ 146:    :F:  $norm62 = \"__lt__\";" ^
      "\n@ 148:    :F:  $norm64 = *get_call($norm37);" ^
      "\n@ 150:    :F:  $norm63 = $norm64;" ^
      "\n@ 152:    :F:  $norm65 = $norm63($norm62);" ^
      "\n@ 154:    :F:  $norm39 = $norm65;" ^
      "\n@ 157:    :F:  goto 156;" ^
      "\n@ 158:    :F:  pass;" ^
      "\n@ 155:    :F:  raise $norm40;" ^
      "\n@ 156:    :F:  pass;" ^
      "\n@  87:    :F:  pass;" ^
      "\n@ 162:    :F:  $norm67 = *get_call($norm39);" ^
      "\n@ 164:    :F:  $norm66 = $norm67;" ^
      "\n@ 166:    :F:  $norm68 = $norm66(z);" ^
      "\n@ 168:    :F:  $norm33 = $norm68;" ^
      "\n@ 172:    :F:  goto 171;" ^
      "\n@ 173:    :F:  pass;" ^
      "\n@ 170:    :F:  $norm33 = $norm32;" ^
      "\n@ 171:    :F:  pass;"
    end
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
    "[1,0.0,'foo',x]"
    begin
      "@   2:    :F:  $norm0 = 1;" ^
      "\n@   4:    :F:  $norm1 = 0.000000;" ^
      "\n@   6:    :F:  $norm2 = \"foo\";" ^
      "\n@   8:    :F:  $norm3 = [$norm0, $norm1, $norm2, x];"
    end
;;

let tuple_test = gen_module_test "tuple_test"
    "(1,0.0,'foo',x)"
    begin
      "@   2:    :F:  $norm0 = 1;" ^
      "\n@   4:    :F:  $norm1 = 0.000000;" ^
      "\n@   6:    :F:  $norm2 = \"foo\";" ^
      "\n@   8:    :F:  $norm3 = ($norm0, $norm1, $norm2, x);"
    end

;;

let if_test = gen_module_test "if_test"
    "if x: z = 1\nelif y: z = -1\nelse: 0"
    begin
      "@   2:    :F:  $norm0 = *bool;" ^
      "\n@   4:    :F:  $norm1 = $norm0(x);" ^
      "\n@  30:    :F:  goto 29 if not $norm1;" ^
      "\n@   6:    :F:  $norm2 = 1;" ^
      "\n@   8:    :F:  $simp1 = $norm2;" ^
      "\n@  10:    :F:  z = $simp1;" ^
      "\n@  28:    :F:  goto 27;" ^
      "\n@  29:    :F:  pass;" ^
      "\n@  12:    :F:  $norm3 = *bool;" ^
      "\n@  14:    :F:  $norm4 = $norm3(y);" ^
      "\n@  26:    :F:  goto 25 if not $norm4;" ^
      "\n@  16:    :F:  $norm5 = -1;" ^
      "\n@  18:    :F:  $simp0 = $norm5;" ^
      "\n@  20:    :F:  z = $simp0;" ^
      "\n@  24:    :F:  goto 23;" ^
      "\n@  25:    :F:  pass;" ^
      "\n@  22:    :F:  $norm6 = 0;" ^
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
      "\n@   6:    :F:  $norm3 = 1;" ^
      "\n@   8:    :F:  $norm0 = $norm3;" ^
      "\n@  28:    :F:  goto 27;" ^
      "\n@  29:    :F:  pass;" ^
      "\n@  10:    :F:  $norm5 = *bool;" ^
      "\n@  12:    :F:  $norm6 = $norm5(y);" ^
      "\n@  24:    :F:  goto 23 if not $norm6;" ^
      "\n@  14:    :F:  $norm7 = -1;" ^
      "\n@  16:    :F:  $norm4 = $norm7;" ^
      "\n@  22:    :F:  goto 21;" ^
      "\n@  23:    :F:  pass;" ^
      "\n@  18:    :F:  $norm8 = 0;" ^
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
      "@   4:   1:F:  $norm1 = 5;" ^
      "\n@   6:   1:F:  $simp0 = $norm1;" ^
      "\n@   8:   1:F:  x = $simp0;" ^
      "\n@  10:   1:F:  $norm3 = *get_call(somefunction);" ^
      "\n@  12:   1:F:  $norm2 = $norm3;" ^
      "\n@  14:   1:F:  $norm4 = $norm2();" ^
      "\n@  50:    :F:  goto 2;" ^
      "\n@   1:    :F:  catch $norm0;" ^
      "\n@  16:    :F:  $norm5 = *type;" ^
      "\n@  18:    :F:  $norm6 = $norm5($norm0);" ^
      "\n@  20:    :F:  $norm7 = $norm6 is ValueError;" ^
      "\n@  22:    :F:  $norm8 = *bool;" ^
      "\n@  24:    :F:  $norm9 = $norm8($norm7);" ^
      "\n@  49:    :F:  goto 48 if not $norm9;" ^
      "\n@  26:    :F:  e = $norm0;" ^
      "\n@  27:    :F:  pass;" ^
      "\n@  47:    :F:  goto 46;" ^
      "\n@  48:    :F:  pass;" ^
      "\n@  29:    :F:  $norm10 = *type;" ^
      "\n@  31:    :F:  $norm11 = $norm10($norm0);" ^
      "\n@  33:    :F:  $norm12 = $norm11 is Int;" ^
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
      "@   7:    :F:  $norm0 = def () {" ^
      "\n@   2:    :F:    $simp0 = f;" ^
      "\n@   4:    :F:    f$1_x = $simp0;" ^
      "\n@   5:    :F:    return n;" ^
      "\n};" ^
      "\n@   9:    :F:  f = $norm0;" ^
      "\n@  11:    :F:  $norm2 = *get_call(f);" ^
      "\n@  13:    :F:  $norm1 = $norm2;" ^
      "\n@  15:    :F:  $norm3 = $norm1();" ^
      "\n@  17:    :F:  $simp1 = $norm3;" ^
      "\n@  19:    :F:  x = $simp1;"
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
      "@   7:    :F:  $norm0 = def (f$1_x, f$1_y) {" ^
      "\n@   2:    :F:    $simp0 = f;" ^
      "\n@   4:    :F:    f$1_x = $simp0;" ^
      "\n@   5:    :F:    return n;" ^
      "\n};" ^
      "\n@   9:    :F:  f = $norm0;" ^
      "\n@  11:    :F:  $norm2 = *get_call(f);" ^
      "\n@  13:    :F:  $norm1 = $norm2;" ^
      "\n@  15:    :F:  $norm3 = $norm1(z);" ^
      "\n@  17:    :F:  $simp1 = $norm3;" ^
      "\n@  19:    :F:  x = $simp1;"
    end
;;

let while_test = gen_module_test "while_test"
    "while True:\n\tx = x + 1"
    begin
      "@   1:    :T:  pass;" ^
      "\n@   4:    :T:  $norm0 = true;" ^
      "\n@   6:    :T:  $norm1 = *bool;" ^
      "\n@   8:    :T:  $norm2 = $norm1($norm0);" ^
      "\n@  96:    :T:  goto 2 if not $norm2;" ^
      "\n@  12:   9:T:  $norm8 = \"__getattribute__\";" ^
      "\n@  14:   9:T:  $norm9 = *get_attribute(x, $norm8);" ^
      "\n@  16:   9:T:  $norm3 = $norm9;" ^
      "\n@  18:   9:T:  $norm10 = \"__add__\";" ^
      "\n@  20:   9:T:  $norm12 = *get_call($norm3);" ^
      "\n@  22:   9:T:  $norm11 = $norm12;" ^
      "\n@  24:   9:T:  $norm13 = $norm11($norm10);" ^
      "\n@  26:   9:T:  $norm6 = $norm13;" ^
      "\n@  83:    :T:  goto 10;" ^
      "\n@   9:    :T:  catch $norm7;" ^
      "\n@  28:    :T:  $norm14 = *type;" ^
      "\n@  30:    :T:  $norm15 = $norm14($norm7);" ^
      "\n@  32:    :T:  $norm16 = *AttributeError;" ^
      "\n@  34:    :T:  $norm17 = $norm15 is $norm16;" ^
      "\n@  36:    :T:  $norm18 = *bool;" ^
      "\n@  38:    :T:  $norm19 = $norm18($norm17);" ^
      "\n@  82:    :T:  goto 81 if not $norm19;" ^
      "\n@  40:    :T:  $norm5 = $norm7;" ^
      "\n@  44:  41:T:  $norm21 = \"__getattr__\";" ^
      "\n@  46:  41:T:  $norm22 = *get_attribute(x, $norm21);" ^
      "\n@  48:  41:T:  $norm4 = $norm22;" ^
      "\n@  67:    :T:  goto 42;" ^
      "\n@  41:    :T:  catch $norm20;" ^
      "\n@  50:    :T:  $norm23 = *type;" ^
      "\n@  52:    :T:  $norm24 = $norm23($norm20);" ^
      "\n@  54:    :T:  $norm25 = *AttributeError;" ^
      "\n@  56:    :T:  $norm26 = $norm24 is $norm25;" ^
      "\n@  58:    :T:  $norm27 = *bool;" ^
      "\n@  60:    :T:  $norm28 = $norm27($norm26);" ^
      "\n@  66:    :T:  goto 65 if not $norm28;" ^
      "\n@  61:    :T:  raise $norm5;" ^
      "\n@  64:    :T:  goto 63;" ^
      "\n@  65:    :T:  pass;" ^
      "\n@  62:    :T:  raise $norm20;" ^
      "\n@  63:    :T:  pass;" ^
      "\n@  42:    :T:  pass;" ^
      "\n@  69:    :T:  $norm29 = \"__add__\";" ^
      "\n@  71:    :T:  $norm31 = *get_call($norm4);" ^
      "\n@  73:    :T:  $norm30 = $norm31;" ^
      "\n@  75:    :T:  $norm32 = $norm30($norm29);" ^
      "\n@  77:    :T:  $norm6 = $norm32;" ^
      "\n@  80:    :T:  goto 79;" ^
      "\n@  81:    :T:  pass;" ^
      "\n@  78:    :T:  raise $norm7;" ^
      "\n@  79:    :T:  pass;" ^
      "\n@  10:    :T:  pass;" ^
      "\n@  85:    :T:  $norm33 = 1;" ^
      "\n@  87:    :T:  $norm35 = *get_call($norm6);" ^
      "\n@  89:    :T:  $norm34 = $norm35;" ^
      "\n@  91:    :T:  $norm36 = $norm34($norm33);" ^
      "\n@  93:    :T:  $simp0 = $norm36;" ^
      "\n@  95:    :T:  x = $simp0;" ^
      "\n@  97:    :T:  goto 1;" ^
      "\n@   2:    :T:  pass;"
    end
;;

let break_test = gen_module_test "break_test"
    "while True:\n\tbreak"
    begin
      "@   1:    :T:  pass;" ^
      "\n@   4:    :T:  $norm0 = true;" ^
      "\n@   6:    :T:  $norm1 = *bool;" ^
      "\n@   8:    :T:  $norm2 = $norm1($norm0);" ^
      "\n@  10:    :T:  goto 2 if not $norm2;" ^
      "\n@   9:    :T:  goto 2;" ^
      "\n@  11:    :T:  goto 1;" ^
      "\n@   2:    :T:  pass;"
    end
;;

let continue_test = gen_module_test "continue_test"
    "while True:\n\tcontinue"
    begin
      "@   1:    :T:  pass;" ^
      "\n@   4:    :T:  $norm0 = true;" ^
      "\n@   6:    :T:  $norm1 = *bool;" ^
      "\n@   8:    :T:  $norm2 = $norm1($norm0);" ^
      "\n@  10:    :T:  goto 2 if not $norm2;" ^
      "\n@   9:    :T:  goto 1;" ^
      "\n@  11:    :T:  goto 1;" ^
      "\n@   2:    :T:  pass;"

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
          parse_to_normalized prog 1 true);
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
