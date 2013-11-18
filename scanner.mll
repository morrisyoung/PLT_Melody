{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '^'      { CARET }
| '+'      { PLUS }
| '*'      { TIMES }
| '='      { ASSIGN }
| "&"      { SYNTHESIZE }
| "<-"     { CONCAT }
| "."      { DOT }
| "=="     { EQ }
| "!="     { NEQ }
| "!"      { NOT }
| "&&"     { AND }
| "||"     { OR }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "break"  { BREAK }
| "return" { RETURN }
| "function"{ FUNCTION }
| "main"   { MAIN }
| ".at" as lxm { M_AT(lxm) }
| ".toneUp" | ".tone.Down" as lxm { M_UPDN(lxm) }
| ".length" as lxm { M_LEN(lxm) }
| "int" | "string" | "bool" | "note" | "ryhthm" | "melody" | "void" as typ { TYPE(typ) }
| "bar"    { BAR }
| "track"  { TRACK }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| '"'['a'-'z' 'A'-'Z']+'"' as lxm { STR(lxm) }
| '~'['A'-'G']['1'-'7']['b' '#']? as lxm { NOTE_VALUE(lxm) }
| "true" | "false" as bool_val { BOOL_VALUE(bool_val) }
| "null"   { NULL }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
