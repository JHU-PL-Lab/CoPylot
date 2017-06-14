exception Parse_error of int

let parse_from_lexbuf lexbuf =
  try
    Python2_pys_generated_parser.file_input Python2_pys_generated_lexer.token lexbuf
  with
  | Python2_pys_generated_parser.Error ->
    raise (Parse_error( lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum))

let parse_stmt_from_lexbuf lexbuf =
  try
    Python2_pys_generated_parser.stmt_input Python2_pys_generated_lexer.token lexbuf
  with
  | Python2_pys_generated_parser.Error ->
    raise (Parse_error( lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum))

let parse_expr_from_lexbuf lexbuf =
  try
    Python2_pys_generated_parser.expr_input Python2_pys_generated_lexer.token lexbuf
  with
  | Python2_pys_generated_parser.Error ->
    raise (Parse_error( lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum))


let parse_from_string s =
  parse_from_lexbuf (Lexing.from_string s)

let parse_stmt_from_string s =
  parse_stmt_from_lexbuf (Lexing.from_string s)

let parse_expr_from_string c =
  parse_expr_from_lexbuf (Lexing.from_string c)
