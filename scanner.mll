{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| "[-t"	   { LBRACKETT}
| "[-r"	   { LBRACKETR}
| "[-b"    { LBRACKETB}
| ']'      { RBRACKET }
| '{'      { LBRACE }
| '}'      { RBRACE }
| "<<"      { LABRACKET }
| ">>"      { RABRACKET }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '*'      { TIMES }
| '='      { ASSIGN }
| '&'      { SYNTHESIZE }
| "<-"     { CONCAT }
| "=="     { EQ }
| "!="     { NEQ }
| "&&"     { AND }
| "||"     { OR }
| '<'      { LT }
| "<="     { LEQ }
| '>'      { GT }
| ">="     { GEQ }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
(*| "break"  { BREAK }  *)
| "return" { RETURN }
| "function"{ FUNCTION }
| "int" | "string" | "bool" | "pitch" | "note" | "rhythm" | "melody" | "void" | "bar" as typ { TYPE(typ) }
| "piano" | "banjo" | "drums" | "clarinet" | "sax" | "guitar" | "violin" | "french horn" | "goblins" | "cello" as lxm { INSTRU(lxm) }
| "track" as lxm { TRACK(lxm) }
| "true" | "false" as bool_val { BOOL_VALUE(bool_val) }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| ('"')[^ '"']+('"') as lxm { STR(lxm) }
| '~'(['A'-'G']['b' '#']?['1'-'7']?)? as lxm { PITCH_VALUE(lxm) }
| "null"  as lxm { NULL(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
