%{
  open Python2_ast_types
  open Python2_abstract_ast
  open Python2_pys_utils
%}

/* literals */
%token <string> NAME
%token <Python2_abstract_ast.number> NUM
%token <bool> BOOL
%token NONE
/*%token <string> STR*/

/* keywords */

%token DEF
%token RETURN
%token PRINT
%token RAISE
%token CATCH
%token PASS
%token GOTO
%token IFN
%token BI_SLICE
%token BI_BOOL
%token BI_TYPE

/* annotations */
%token ANNOT_EXPR
%token ANNOT_STMT

/* symbols */
%token  EQ
%token  GE
%token  QUOTE
%token  LPAREN
%token  RPAREN
%token  LBRACK
%token  RBRACK
%token  LBRACE
%token  RBRACE
%token  COLON
%token  SEMICOLON
%token  DOT
%token  COMMA
/*%token <Python2_abstract_ast.sign> SGN*/
%token  END

/* other */
%token <Python2_ast_types.uid> UID
%token <bool> LOOP

%start file_input
%type <Python2_abstract_ast.modl> file_input

%start stmt_input
%type <Python2_abstract_ast.annotated_stmt> stmt_input

%start expr_input
%type <Python2_abstract_ast.annotated_expr> expr_input


%%
file_input:
  | stmt_list END { let () = reset_uid () in Module ($1,0) }

stmt_input:
  /*1:2:true:<stmt>;*/
  | stmt_annot stmt SEMICOLON END
    { let () = reset_uid () in $1 $2 }

expr_input:
  | expr_annot expr END
    { let () = reset_uid () in $1 $2 }

%inline
stmt_annot:
  | ANNOT_STMT UID COLON except COLON LOOP COLON
    { fun x -> {uid=$2;exception_target=$4;multi=$6;body=x} }
  | { fun x -> {uid=next_uid ();exception_target=None;multi=false;body=x} }

%inline
expr_annot:
  | ANNOT_EXPR UID COLON except COLON LOOP COLON
    { fun x -> {uid=$2;exception_target=$4;multi=$6;body=x} }
  | { fun x -> {uid=next_uid ();exception_target=None;multi=false;body=x} }

except:
  | {None}
  | UID {Some($1)}

stmt_list:
  | { [] }
  | stmt_annot stmt SEMICOLON stmt_list
    { $1 $2 :: $4 }

stmt:
  | assign {$1}
  | return {$1}
  | print {$1}
  | RAISE NAME { Raise($2) }
  | CATCH NAME { Catch($2) }
  | PASS { Pass }
  | GOTO UID IFN NAME { GotoIfNot($4,$2) }
  | GOTO UID { Goto($2) }

assign:
  /*<name> = <expr>*/
  | NAME EQ expr_wrapper { Assign($1,$3) }

return:
  | RETURN NAME { Return($2) }

print:
  | PRINT lst GE NAME { Print(Some($4),$2,false) }
  | PRINT lst { Print(None,$2,false) }

lst:
  | { [] }
  | NAME { [$1] }
  | NAME COMMA lst { $1::$3 }

expr_wrapper:
  | expr_annot expr
    { $1 $2 }

expr:
  | NAME LPAREN lst RPAREN { Call($1,$3) }
  | NAME DOT NAME { Attribute($1,$3) }
  | LBRACK lst RBRACK { List($2) }
  | LPAREN lst RPAREN { Tuple($2) }
  | literal { Literal($1) }
  | NAME { Name($1) }
  | NONE { NoneExpr }

literal:
  | NUM { Num($1) }
  | QUOTE NAME QUOTE { Str(StringLiteral($2)) }
  | BOOL { Bool($1) }
  | BI_SLICE { Builtin(Builtin_slice) }
  | BI_BOOL { Builtin(Builtin_bool) }
  | BI_TYPE { Builtin(Builtin_type) }
  | funcval {$1}

funcval:
  /*def (<params>){<stmt_list>}*/
  | DEF LPAREN lst RPAREN LBRACE stmt_list RBRACE
    { FunctionVal($3,$6) }
