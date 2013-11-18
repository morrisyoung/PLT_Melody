%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA
%token CARET PLUS TIMES ASSIGN SYNTHESIZE CONCAT DOT
%token EQ NEQ NOT AND OR LT LEQ GT GEQ
%token IF ELSE FOR WHILE BREAK RETURN
%token DEFINE FUNCTION NULL MAIN
%token <string> M_AT
%token <string> M_UPDN
%token <string> M_LEN
%token <string> TYPE
%token <string> BAR
%token <string> TRACK
%token <int> LITERAL
%token <string> ID
%token <string> STR
%token <string> NOTE_VALUE
%token <string> BOOL_VALUE
%token <string> NULL
%token EOF

%nonassoc ELSE
%right ASSIGN CONCAT
%left EQ NEQ
%left LT GT LEQ GEQ
%left CARET PLUS MINUS SYNTHESIZE
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

program:
   /* nothing */ { [], [] }
 | program vdecl { ($2 :: fst $1), snd $1 }
 | program fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   FUNCTION TYPE func_name LPAREN formals_list RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { rtype= $2;
         fname = $3;
	 formals = List.rev $5;
	 locals = List.rev $8;
	 body = List.rev $9 } }

func_name:
    MAIN|ID   { $1 }

formals_list:
    /* nothing */ { [] }
  | formals_list COMMA formal  { $2 :: $1 }

formal:
    TYPE ID   { $2 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    TYPE ID SEMI   { $2 }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
  | BREAK SEMI  { Break }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    TYPE ID          { V_def($1,$2) }
  | BAR LPAREN note_opt RPAREN { Bar_def($3) }
  | LPAREN expr SEMI expr RPAREN  { Tuple($2,$4) }
  | LBRACKET tuple_opt_1 RBRACKET { Bar_val_1($2) }
  | LBRACKET expr COMMA LPAREN tuple_opt_2 RPAREN RBRACKET { Bar_val_2($2,$4) }
  | LBRACKET actuals_opt RBRACKET { Rhy_val($2) }
  | TRACK LPAREN actuals_opt RPAREN { Track_def($3) }
  | LBRACE actuals_opt RBRACE   { Track_val($2) }
  | LITERAL          { Literal($1) }
  | NOTE_VALUE       { Note_value($1) }
  | STR              { Str($1) }
  | BOOL_VALUE       { Bool($1) }
  | NULL             { Null($1) }
  | ID               { Id($1) }
  | ID M_AT LPAREN LITERAL RPAREN SEMI { M_at($1,$4) }
  | ID M_UPDN LPAREN LITERAL RPAREN SEMI { M_updn($1,$2,$4) }
  | ID M_LEN LPAREN RPAREN SEMI { M_len($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr CARET  expr { Binop($1, Conn,  $3) }
  | expr SYNTHESIZE expr { Binop($1, Paral, $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | NOT expr         { Not($2) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | expr CONCAT expr { Concat($1, $3) }
  | expr ASSIGN expr   { Assign($1, $3) }
  | expr LPAREN actuals_opt RPAREN SEMI { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

note_opt:
    /* nothing */ { [] }
  | note_list  { List.rev $1 }

note_list:
    expr                    { [$1] }
  | note_list SYNTHESIZE expr { $3 :: $1 }

tuple_opt_1:
    /* nothing */ { [] }
  | tuple_list_1  { List.rev $1 }

tuple_list_1:
    expr                       { [$1] }
  | tuple_list_1 COMMA expr    { $3::$1 }

tuple_opt_2:
    /* nothing */ { [] }
  | tuple_list_2  { List.rev $1 }

tuple_list_2:
    expr                    { [$1] }
  | tuple_list_2 COMMA expr { $3 :: $1 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
