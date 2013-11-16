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
| "int"    { INT}
| "string" { STRING }
| "bool"   { BOOL }
| "true"   { TRUE }
| "false"  { FALSE }
| "note"   { NOTE }
| "bar"    { BAR }
| "rhythm" { RHYTHM }
| "track"  { TRACK }
| "melody" { MELODY }
| "define" { DEFINE }
| "function"{ FUNCTION }
| "null"   { NULL }
| "void"   { VOID }
| "main"   { MAIN }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| '.'['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm  { METHOD(lxm) }
| '"'['a'-'z' 'A'-'Z']+'"' as lxm { STR(lxm) }
| '~'['A'-'G']['1'-'7']['b'|'#']? as lxm { NOTE_VALUE(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
