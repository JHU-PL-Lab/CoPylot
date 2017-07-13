%{
  open Lamia_ast;;
  open Lamia_generated_parser_utils;;
%}

/* payload tokens */
%token <string> IDENT
%token <string> MEMIDENT
%token <int> INTEGER
%token <bool> BOOL
%token <string> STRING

/* symbols */
%token SEMICOLON
%token COLON
%token EQUAL
%token OPEN_PAREN
%token CLOSE_PAREN
%token OPEN_BRACE
%token CLOSE_BRACE
%token ARROW
%token COMMA
%token AT

/* keywords */
%token LET
%token ALLOC
%token STORE
%token GET
%token IS
%token RETURN
%token RAISE
%token TRY
%token EXCEPT
%token IF
%token THEN
%token ELSE
%token WHILE
%token DEF
%token NONE

/* operators */
%token NOT
%token HASKEY
%token INTPLUS
%token INTMINUS

/* other */
%token EOF

/* starting ponts */
%start file_input
%type <Lamia_ast.uid Lamia_ast.block> file_input

%start statement_input
%type <Lamia_ast.uid Lamia_ast.statement> statement_input

%start value_expression_input
%type <Lamia_ast.uid Lamia_ast.value_expression> value_expression_input

%%
file_input:
  | statement_list EOF
    { reset_uid (); Block($1) }

statement_input:
  | statement SEMICOLON? EOF
    { reset_uid (); $1 }

value_expression_input:
  | value_expression EOF
    { reset_uid (); $1 }

block:
  | OPEN_BRACE statement_list CLOSE_BRACE
    { Block($2) }

statement_list:
  | list(terminated(statement,SEMICOLON))
    { $1 }

statement:
  | AT label COLON directive
    { Statement($2,$4) }
  | directive
    { Statement(next_uid(),$1) }

label:
  | INTEGER
    { $1 }

directive:
  | LET value_variable EQUAL value_expression
    { Let_expression($2,$4) }
  | LET memory_variable EQUAL ALLOC
    { Let_alloc($2) }
  | LET value_variable EQUAL value_variable
    { Let_alias_value($2,$4) }
  | LET memory_variable EQUAL memory_variable
    { Let_alias_memory($2,$4) }
  | LET value_variable EQUAL
      value_variable OPEN_BRACE value_variable ARROW memory_variable CLOSE_BRACE
    { Let_binding_update($2,$4,$6,$8) }
  | LET memory_variable EQUAL
      value_variable OPEN_BRACE value_variable CLOSE_BRACE
    { Let_binding_access($2,$4,$6) }
  | LET memory_variable EQUAL value_variable
      OPEN_PAREN value_variable_list CLOSE_PAREN
    { Let_call_function($2,$4,$6) }
  | STORE memory_variable value_variable
    { Store($2,$3) }
  | LET value_variable EQUAL GET memory_variable
    { Let_get($2,$5) }
  | LET value_variable EQUAL memory_variable IS memory_variable
    { Let_is($2,$4,$6) }
  | LET value_variable EQUAL unary_operator value_variable
    { Let_unop($2,$4,$5) }
  | LET value_variable EQUAL value_variable binary_operator value_variable
    { Let_binop($2,$4,$5,$6) }
  | RETURN memory_variable
    { Return($2) }
  | RAISE memory_variable
    { Raise($2) }
  | TRY block EXCEPT memory_variable block
    { Try_except ($2,$4,$5) }
  | LET value_variable EQUAL IF value_variable THEN block ELSE block
    { Let_conditional_value($2,$5,$7,$9) }
  | LET memory_variable EQUAL IF value_variable THEN block ELSE block
    { Let_conditional_memory($2,$5,$7,$9) }
  | WHILE memory_variable block
    { While($2,$3) }

%inline
value_variable:
  | IDENT
    { Value_variable($1) }

%inline
memory_variable:
  | MEMIDENT
    { Memory_variable($1) }

value_variable_list:
  | separated_list_trailing(COMMA, value_variable)
    { $1 }

value_expression:
  | INTEGER
    { Integer_literal($1) }
  | STRING
    { String_literal($1) }
  | BOOL
    { Boolean_literal($1) }
  | DEF OPEN_PAREN value_variable_list CLOSE_PAREN block
    { Function_expression($3,$5) }
  | NONE
    { None_literal }

%inline
unary_operator:
  | NOT
    { Unop_not }

%inline
binary_operator:
  | INTPLUS
    { Binop_intplus }
  | INTMINUS
    { Binop_intminus }
  | HASKEY
    { Binop_haskey }

%inline
separated_list_trailing(sep,item):
  | separated_list(sep,item) sep?
    { $1 }
