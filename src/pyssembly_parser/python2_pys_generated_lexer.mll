  {
    open Python2_pys_generated_parser
  }


  (* Some sample programs:
  1::0:x=y;
  2::0:goto(3);
  3::0:f()
  {
  5::0:return(int)
  }

  *)

  let digit = ['0'-'9']
  let decimalinteger = digit+
  let intpart = digit+
  let fraction = '.' digit+
  let pointfloat = intpart? fraction | intpart '.'
  let exponent = ['e' 'E'] ['+' '-']? digit+
  let exponentfloat = (intpart | pointfloat) exponent
  let floatnumber = pointfloat | exponentfloat
  let identifier = ['a'-'z' 'A'-'Z' '_' '$'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '$']*


  rule token = parse
    | [' ' '\n' '\t'] { token lexbuf }
    | ';' { SEMICOLON }
    | ':' { COLON }

    (* UID will not start with 0 *)

    | 'T' { LOOP (true) }
    | 'F' { LOOP (false) }

    | "@e" {ANNOT_EXPR}
    | '@' {ANNOT_STMT}

    | '(' { LPAREN }
    | ')' { RPAREN }
    | '[' { LBRACK }
    | ']' { RBRACK }
    | '{' { LBRACE }
    | '}' { RBRACE }
    | '.' { DOT }
    | ',' { COMMA }
    | '=' { EQ }
    | '>' { GE }
    | '\"' { QUOTE }
    | eof { END }
    | decimalinteger as n
        { INT (int_of_string n) }
    | floatnumber as n
        { FLOAT (float_of_string n) }
    | identifier as id
      { match id with
        | "def" -> DEF
        | "return" -> RETURN
        | "print" -> PRINT
        | "raise" -> RAISE
        | "catch" -> CATCH
        | "pass" -> PASS
        | "goto" -> GOTO
        | "ifnot" -> IFN
        | "slice" -> BI_SLICE
        | "bool" -> BI_BOOL
        | "type" -> BI_TYPE
        | "true" -> BOOL (true)
        | "false" -> BOOL (false)
        | _ -> NAME (id)
      }
