%{
  open Python2_normalized_ast
  open Python2_pys_utils
%}

/* literals */
%token <string> NAME
%token <Python2_normalized_ast.number> NUM
%token <bool> BOOL
/*%token <string> STR*/

/* keywords */

%token DEF
%token RETURN
%token PRINT
%token RAISE
%token CATCH
%token PASS
%token GOTO
%token GOTOIFN
%token BI_SLICE
%token BI_BOOL
%token BI_TYPE

/* annotations */
%token ANNOT_SEXPR
%token ANNOT_CEXPR
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
/*%token <Python2_normalized_ast.sign> SGN*/
%token  END

/* other */
%token <int> UID
%token <bool> LOOP

%start file_input
%type <Python2_normalized_ast.modl> file_input

%start stmt_input
%type <Python2_normalized_ast.annotated_stmt> stmt_input

%start cexpr_input
%type <Python2_normalized_ast.annotated_cexpr> cexpr_input

%start sexpr_input
%type <Python2_normalized_ast.annotated_sexpr> sexpr_input

%%
file_input:
  | stmt_list END { let () = reset_uid () in Module ($1,0) }

stmt_input:
  /*1:2:true:<stmt>;*/
  | stmt_annot stmt SEMICOLON END
    { let () = reset_uid () in $1 $2 }

cexpr_input:
  | cexpr_annot cexpr END
    { let () = reset_uid () in $1 $2 }

sexpr_input:
  | sexpr END { let () = reset_uid () in $1 }

stmt_annot:
  | ANNOT_STMT UID COLON except COLON LOOP COLON
    { fun x -> {uid=$2;exception_target=$4;multi=$6;body=x} }
  | { fun x -> {uid=next_uid ();exception_target=None;multi=false;body=x} }

cexpr_annot:
  | ANNOT_CEXPR UID COLON except COLON LOOP COLON
    { fun x -> {uid=$2;exception_target=$4;multi=$6;body=x} }
  | { fun x -> {uid=next_uid ();exception_target=None;multi=false;body=x} }

sexpr_annot:
  | ANNOT_SEXPR UID COLON except COLON LOOP COLON
    { fun x -> {uid=$2;exception_target=$4;multi=$6;body=x} }
  | { fun x -> {uid=next_uid ();exception_target=None;multi=false;body=x} }

stmt_list:
  | { [] }
  | stmt_annot stmt SEMICOLON stmt_list
    { $1 $2 :: $4 }

stmt:
  | assign {$1}
  | funcdef {$1}
  | return {$1}
  | print {$1}
  | RAISE sexpr { Raise($2) }
  | CATCH NAME { Catch($2) }
  | PASS { Pass }
  | GOTO UID { Goto($2) }
  | GOTOIFN sexpr UID { GotoIfNot($2,$3) }
  | sexpr { SimpleExprStmt($1) }

assign:
  /*<name> = <cexpr>*/
  | NAME EQ cexpr_wrapper { Assign($1,$3) }

funcdef:
  /*<name>(<params>){<stmt_list>}*/
  | DEF NAME LPAREN id_lst RPAREN EQ LBRACE stmt_list RBRACE
    { FunctionDef($2,$4,$8) }

return:
  | RETURN { Return(None) }
  | RETURN sexpr { Return(Some($2)) }

print:
  | PRINT lst GE sexpr { Print(Some($4),$2,false) }
  | PRINT lst { Print(None,$2,false) }

id_lst:
  | { [] }
  | NAME { [$1] }
  | NAME COMMA id_lst { $1::$3 }

lst:
  | { [] }
  | sexpr { [$1] }
  | sexpr COMMA lst { $1::$3 }

cexpr_wrapper:
  | cexpr_annot cexpr
    { $1 $2 }


cexpr:
  | sexpr LPAREN lst RPAREN { Call($1,$3) }
  | sexpr DOT NAME { Attribute($1,$3) }
  | LBRACK lst RBRACK { List($2) }
  | LPAREN lst RPAREN { Tuple($2) }
  | sexpr { SimpleExpr($1) }


sexpr:
  | sexpr_annot literal
    { $1 (Literal($2)) }
  | sexpr_annot NAME
    { $1 (Name($2)) }

literal:
  | NUM { Num($1) }
  | QUOTE NAME QUOTE { Str(StringLiteral($2)) }
  | BOOL { Bool($1) }
  | BI_SLICE { Builtin(Builtin_slice) }
  | BI_BOOL { Builtin(Builtin_bool) }
  | BI_TYPE { Builtin(Builtin_type) }

except:
  | {None}
  | UID {Some($1)}
