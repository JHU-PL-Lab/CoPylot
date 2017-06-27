{
  open Python2_abs_generated_parser
  open Python2_abstract_ast
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
let identifier = ['a'-'z' 'A'-'Z' '_' '$'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '$' '+' '-']*


rule token = parse
  | [' ' '\n' '\t'] { token lexbuf }
  | ';' { SEMICOLON }
  | ':' { COLON }

  (* UID will not start with 0 *)

  | ['1'-'9'] digit* as u { UID (int_of_string u) }
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
      | "Int+" -> NUM (Int(Pos))
      | "Int-" -> NUM (Int(Neg))
      | "Int0" -> NUM (Int(Zero))
      | "Float+" -> NUM (Float(Pos))
      | "Float-" -> NUM (Float(Neg))
      | "Float0" -> NUM (Float(Zero))
      | _ -> NAME (id)
    }
