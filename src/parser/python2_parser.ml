exception Parse_error of Lexing.position

let parse_from_lexbuf lexbuf =
  let state = Python2_generated_lexer_state.create () in
  try
    Python2_generated_parser.file_input (Python2_generated_lexer.token state) lexbuf
  with
  | Python2_generated_parser.Error ->
    raise (Parse_error(lexbuf.Lexing.lex_curr_p))

let parse_from_string s =
  parse_from_lexbuf (Lexing.from_string s)

let parse_from_channel c =
  parse_from_lexbuf (Lexing.from_channel c)

let parse_from_file file =
  let c = open_in file in
  try
    let ast = parse_from_channel c in
    close_in c;
    ast
  with e ->
    close_in c;
    raise e

let parse_stmt_from_lexbuf lexbuf =
  let state = Python2_generated_lexer_state.create () in
  try
    Python2_generated_parser.stmt_input (Python2_generated_lexer.token state) lexbuf
  with
  | Python2_generated_parser.Error ->
    raise (Parse_error(lexbuf.Lexing.lex_curr_p))

let parse_stmt_from_string s =
  parse_stmt_from_lexbuf (Lexing.from_string s)

let parse_stmt_from_channel c =
  parse_stmt_from_lexbuf (Lexing.from_channel c)
