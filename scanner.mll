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
| '<'      { LABRACKET }
| '>'      { RABRACKET }
| ';'      { SEMI }
| ','      { COMMA }
| '^'      { CARET }
| '+'      { PLUS }
| '*'      { TIMES }
| '='      { ASSIGN }
| '&'      { SYNTHESIZE } (*getong from "" to ''*)
| "<-"     { CONCAT }
| "=="     { EQ }
| "!="     { NEQ }
| '!'      { NOT } (*getong from "" to ''*)
| "&&"     { AND }
| "||"     { OR }
| '<'      { LT }
| "<="     { LEQ }
| '>'      { GT } (*getong from "" to ''*)
| ">="     { GEQ }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "break"  { BREAK }
| "return" { RETURN }
| "function"{ FUNCTION }
| "main" as lxm  { MAIN(lxm) }
| "int" | "string" | "bool" | "pitch" | "note" | "ryhthm" | "melody" | "void" as typ { TYPE(typ) }
| "bar" as lxm   { BAR(lxm) }
| "track" as lxm { TRACK(lxm) }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| '"'['a'-'z' 'A'-'Z']+'"' as lxm { STR(lxm) }
| '~'(['A'-'G']['b' '#']?['1'-'7']?)? as lxm { PITCH_VALUE(lxm) }
| "true" | "false" as bool_val { BOOL_VALUE(bool_val) }
| "null"  as lxm { NULL(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
