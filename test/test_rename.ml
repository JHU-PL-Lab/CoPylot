open OUnit2
open Batteries
open Jhupllib
open Python2_parser
open Python2_ast
open Lexing
open Python2_rename_ast

(* TODO: *)
(* - Ignore keyword *)
(* - Fix Param and Store conflicts *)

let id_map = Id_map.empty;;


let annot = Pos.of_pos Lexing.dummy_pos;;

let string_of_stmt e  = Pp_utils.pp_to_string
    (Pp_utils.pp_list (Python2_ast.pp_stmt (fun _ _ -> ()))) e;;
let equivalent_stmt e1 e2 = List.eq (equal_stmt ( fun _ _ -> true)) e1 e2;;

let string_of_modl m = Pp_utils.pp_to_string
    (Python2_ast.pp_modl (fun _ _ -> ())) m;;
let equivalent_modl m1 m2 = equal_modl ( fun _ _ -> true) m1 m2;;

let parse_stmt_from_string_safe str =
  try
    parse_stmt_from_string str
  with
  | Python2_parser.Parse_error p ->
    assert_failure (Printf.sprintf "Error in line %d, col %d."
                      p.pos_lnum p.pos_cnum)
;;

let parse_from_string_safe str =
  try
    parse_from_string str
  with
  | Python2_parser.Parse_error p ->
    assert_failure (Printf.sprintf "Error in line %d, col %d."
                      p.pos_lnum p.pos_cnum)
;;

(* Functions to hide testing boilerplate *)

let gen_module_test (name : string) (prog : string) (expected : 'a stmt list) =
  name>::
  ( fun _ ->
      let concrete = parse_from_string_safe (prog ^ "\n") in
      let renamed = rename_modl (id_map) [] concrete in
      assert_equal ~printer:string_of_modl ~cmp:equivalent_modl
        (Module(expected, annot)) renamed
  )

let gen_stmt_test (name : string) (prog : string) (expected : 'a expr) =
  name>::
  ( fun _ ->
      let concrete = parse_stmt_from_string_safe (prog ^ "\n") in
      let renamed = rename_stmt_list (id_map) [] concrete in
      assert_equal ~printer:string_of_stmt ~cmp:equivalent_stmt
        [(Expr(expected, annot))] renamed
  )


let simple_func =
  "x=1" ^
  "\ndef f():" ^
  "\n\tx=0"

let simple_test = gen_module_test "simple_test"
    simple_func
    [Assign(
        [
          Name("x", Store, annot)
        ],
        Num((Int 1), annot),
        annot
      );
     FunctionDef
       ("f",
        ([], None, None, []),
        [Assign(
            [
              Name ("f$2_x", Store, annot)
            ],
            Num((Int 0), annot),
            annot)
        ],[], annot)]
;;

let evil_func =
  "def f(f):" ^
  "\n\tdef f():" ^
  "\n\t\treturn f" ^
  "\n\treturn f" ^
  "\nf=f(f)"

let evil_test = gen_module_test "evil_test"
    evil_func
    [FunctionDef
       ("f",
        ([
          Name ("f$1_f", Param, annot)
        ], None, None, []),
        [FunctionDef
           ("f$1_f",
            ([], None, None, []),
            [Return(
                Some(Name("f$1_f", Load, annot)),
                annot)
            ],[], annot);
         Return(
           Some(Name("f$1_f", Load, annot)),
           annot)
        ],[], annot);
     Assign(
       [
         Name("f", Store, annot)
       ],
       Call(
         Name("f", Load, annot),
         [Name("f", Load, annot)],
         [], None, None, annot
       ),
       annot
     );
    ]
;;

let func_in_if =
  "x=True" ^
  "\ndef f():" ^
  "\n\tif x:" ^
  "\n\t\tdef f():" ^
  "\n\t\t\treturn x" ^
  "\n\tx=0"

let func_in_if_test = gen_module_test "func_in_if_test"
    func_in_if
    [
      Assign(
        [
          Name ("x", Store, annot)
        ],
        Bool(true, annot),
        annot);
      FunctionDef(
        "f",
        ([], None, None, []),
        [
          If(
            Name ("f$2_x", Load, annot),
            [
              FunctionDef(
                "f$2_f",
                ([], None, None, []),
                [
                  Return(
                    Some(Name("f$2_x", Load, annot)),
                    annot)
                ],
                [], annot);
            ],
            [], annot
          );
          Assign(
            [
              Name ("f$2_x", Store, annot)
            ],
            Num(Int 0, annot),
            annot);
        ],
        [], annot);
    ]
;;

let double_def =
  "def f():" ^
  "\n\tx=0" ^
  "\ndef f():" ^
  "\n\tx=0"

let double_def_test = gen_module_test "double_def_test"
    double_def
    [
      FunctionDef(
        "f",
        ([], None, None, []),
        [
          Assign(
            [Name ("f$1_x", Store, annot)],
            Num(Int 0, annot),
            annot);
        ],
        [], annot);
      FunctionDef(
        "f",
        ([], None, None, []),
        [
          Assign(
            [Name ("f$3_x", Store, annot)],
            Num(Int 0, annot),
            annot);
        ],
        [], annot);
    ]
;;

let double_def_param =
  "def f():" ^
  "\n\tx=0" ^
  "\ndef f(x,y):" ^
  "\n\treturn x"

let double_def_param_test = gen_module_test "double_def_param_test"
    double_def_param
    [
      FunctionDef(
        "f",
        ([], None, None, []),
        [
          Assign(
            [Name ("f$1_x", Store, annot)],
            Num(Int 0, annot),
            annot);
        ],
        [], annot);
      FunctionDef(
        "f",
        ([
          Name ("f$3_x", Param, annot);
          Name ("f$3_y", Param, annot)
        ], None, None, []),
        [
          Return(
            Some(Name("f$3_x", Load, annot)),
            annot);
        ],
        [], annot);
    ]
;;

let book_eg =
  "def f(x):" ^
  "\n\tif x==1:"

let tricky_param =
  "def f(x):" ^
  "\n\tif x:" ^
  "\n\t\tx=False" ^
  "\n\t\treturn x" ^
  "\n\treturn x"

let tricky_param_test = gen_module_test "tricky_param_test"
    tricky_param
    [
      FunctionDef(
        "f",
        ([Name("f$1_x", Param, annot)], None, None, []),
        [If(
            Name("f$1_x", Load, annot),
            [Assign(
                [Name("f$1_x", Store, annot)],
                Bool(false, annot),
                annot
              );
             Return(
               Some(Name("f$1_x", Load, annot)),
               annot);
            ],
            [], annot);
         Return(
           Some(Name("f$1_x", Load, annot)),
           annot);
        ],
        [], annot);
    ]
;;

let tricky_for =
  "def f(x):" ^
  "\n  for x in range(x):" ^
  "\n    print x"

let tricky_for_test = gen_module_test "tricky_for_test"
    tricky_for
    [FunctionDef(
        "f",
        ([Name("f$1_x", Param, annot)],None, None, []),
        [For(
            Name("f$1_x", Store, annot),
            Call(Name ("range", Load, annot),
                 [Name ("f$1_x", Load, annot)],[], None, None, annot
                ),
            [Print(None,
                   [Name("f$1_x", Load, annot)],
                   true, annot)
            ],
            [], annot)
        ],
        [], annot)
    ]
;;

let easy_while =
  "def f(x):" ^
  "\n  while x:" ^
  "\n    x -= 1"

let easy_while_test = gen_module_test "easy_while_test"
    easy_while
    [FunctionDef(
        "f",
        ([Name("f$1_x", Param, annot)],None, None, []),
        [While(
            Name("f$1_x", Load, annot),
            [AugAssign(
                Name("f$1_x", Store, annot),
                Sub,
                Num(Int(1), annot),
                annot
              )
            ],
            [], annot)
        ],
        [], annot)
    ]

let triangle_def =
  "def triangle(n):" ^
     "\n\tcount = 0" ^
     "\n\ti=0" ^
     "\n\twhile count < n:" ^
     "\n\t\ti += count" ^
     "\n\t\tcount = count + 1" ^
     "\n\treturn i" ^
     "\n"
  (* "def t(n):" ^
  (* "\n\tpass" ^ *)
  (* "\n\tn=c" ^ *)
  (* "\n\tc=0" ^ *)
  "\n\tc=n" ^
  (* "\n\tdef n(n):" ^ *)
  (* "\n\t\tn=c" ^ *)
  "\n" *)
;;

let triangle_test = gen_module_test "triangle_test"
    triangle_def
    [FunctionDef(
        "f",
        ([Name("f$1_x", Param, annot)],None, None, []),
        [While(
            Name("f$1_x", Load, annot),
            [AugAssign(
                Name("f$1_x", Store, annot),
                Sub,
                Num(Int(1), annot),
                annot
              )
            ],
            [], annot)
        ],
        [], annot)
    ]

let tests =
  "rename_ast">:::
  [
    simple_test;
    evil_test;
    func_in_if_test;
    double_def_test;
    double_def_param_test;
    tricky_param_test;
    tricky_for_test;
    easy_while_test;
    (* triangle_test; *)
  ]
