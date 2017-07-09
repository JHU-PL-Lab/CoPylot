exception Parse_error of int * int

let parse_from_lexbuf lexbuf =
  try
    Lamia_generated_parser.file_input Lamia_generated_lexer.token lexbuf
  with
  | Lamia_generated_parser.Error ->
    raise (Parse_error( lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum,
                        lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum))

let parse_statement_from_lexbuf lexbuf =
  try
    Lamia_generated_parser.statement_input Lamia_generated_lexer.token lexbuf
  with
  | Lamia_generated_parser.Error ->
    raise (Parse_error( lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum,
                        lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum))

let parse_value_expression_from_lexbuf lexbuf =
  try
    Lamia_generated_parser.value_expression_input Lamia_generated_lexer.token lexbuf
  with
  | Lamia_generated_parser.Error ->
    raise (Parse_error( lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum,
                        lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum))

let parse_from_string s =
  parse_from_lexbuf (Lexing.from_string s)

let parse_statement_from_string s =
  parse_statement_from_lexbuf (Lexing.from_string s)

let parse_value_expression_from_string c =
  parse_value_expression_from_lexbuf (Lexing.from_string c)
