%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACKETT LBRACKETB LBRACKETR RBRACKET LBRACE RBRACE LABRACKET RABRACKET COMMA
%token PLUS TIMES ASSIGN SYNTHESIZE CONCAT
%token EQ NEQ AND OR LT LEQ GT GEQ
%token IF ELSE FOR WHILE BREAK RETURN
%token FUNCTION NULL
%token <string> MAIN
%token <string> METHOD
/*
%token <string> M_AT
%token <string> M_UPDN
%token <string> M_LEN
*/
%token <string> TYPE
%token <string> BAR
%token <string> TRACK
%token <int> LITERAL
%token <string> ID
%token <string> STR
%token <string> PITCH_VALUE
%token <string> BOOL_VALUE
%token <string> NULL
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
/*
%nonassoc LPAREN
%nonassoc LBRACKET
%nonassoc LBRACE
*/
%nonassoc LBRACE
%nonassoc LBRACKET
%nonassoc LPAREN
%left COMMA
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS SYNTHESIZE CONCAT
%left TIMES

%start program
%type <Ast.program> program

%%
program:
   /* nothing */    { [],[] }
 | program var_decl { List.rev ($2 :: List.rev (fst $1)), snd $1 }
 | program func_decl { fst $1, List.rev ($2 :: List.rev (snd $1)) }

func_decl:
   FUNCTION all_type func_name LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { rtype= $2;
         fname = $3;
   formals = $5;
   locals = List.rev $8;
   body = List.rev $9 } }

all_type:
     TYPE  { $1 }
  | BAR    { $1 }
  | TRACK  { $1 }

func_name:
     MAIN  { $1 }
  | ID     { $1 }

formals_opt:
     /* nothing */ { [] }
  | formal_list { List.rev($1) }
 
formal_list:
     par_decl { [$1] }
  | formal_list COMMA par_decl { $3 :: $1 }

par_decl:
     all_type ID  {{ p_name=$2;p_type=$1 }}

vdecl_list:
    /* nothing */     { [] }
  | vdecl_list var_decl { $2 :: $1 }

var_decl:
     TYPE expr SEMI                            {{ v_type=$1;v_init=$2;v_attr=[]}}
  /*| BAR LABRACKET expr RABRACKET ID SEMI     {{ v_type=$1;v_name=$5;v_attr=$3}}*/
  | BAR LABRACKET actuals_opt RABRACKET expr SEMI     {{ v_type=$1;v_init=$5;v_attr=$3}}
  | TRACK LABRACKET actuals_opt RABRACKET expr SEMI  {{ v_type=$1;v_init=$5;v_attr=$3}}

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

     /*LPAREN expr SEMI expr RPAREN { Note_value }*/
     /*LPAREN expr SEMI expr RPAREN { Note_value($2,$4) }*/
     LPAREN expr SEMI LITERAL RPAREN { Note_value($2,$4) } 
  |  LBRACKETT actuals_opt RBRACKET       { Track_value($2)} /*getong changed, solve 1 r/r conflict, bring 1 s/r, not differentiate bar or rhythm right here*/  
  |  LBRACKETB actuals_opt RBRACKET       { Bar_value1($2)} /*getong changed, solve 1 r/r conflict, bring 1 s/r, not differentiate bar or rhythm right here*/  
  |  LBRACKETR actuals_opt RBRACKET       { Rhythm_value($2)} /*getong changed, solve 1 r/r conflict, bring 1 s/r, not differentiate bar or rhythm right here*/  
  
  /*| LBRACKET actuals_opt RBRACKET { Bar_val_1($2) } why opt?*/
  |  LBRACKET expr SEMI LPAREN actuals_opt RPAREN RBRACKET { Bar_value2($2,$5) } /*getong change the $2expr to ID make r/r conflict reduced to 1 from 4; but actually it works after we change it back*/
                                                                              /*change COMMA to SEMI reduce 1 s/r conflict*/
  /*| LBRACKET expr COMMA LPAREN actuals_opt RPAREN RBRACKET { Bar_val_2($2,$5) }*/
  /*|  LBRACKET actuals_rhy_l RBRACKET {Rhy_val($2) }*/
  /*| LBRACKET actuals_rhy RBRACKET { Rhy_val($2) }*/
  /*|  LBRACE actuals_list RBRACE   { Track_val($2) } this line brings 10 s/r*/
  /*| LBRACE actuals_opt RBRACE   { Track_val($2) }*/
  | LITERAL          { Literal($1) }
  | PITCH_VALUE       { Pitch_value($1) }
  | STR              { Str($1) }
  | BOOL_VALUE       { Bool($1) }
  | NULL             { Null($1) }
  | ID               { Id($1) }
  /*| ID DOT ID LPAREN actuals_opt RPAREN  { Call($1, $3, $5)}getong add this line and the below line to substitute M_AT, M_UPDN, M_LEN */
  | ID LPAREN actuals_opt RPAREN { Call($1, $3)}
  /*| ID M_AT LPAREN LITERAL RPAREN   { M_at($1,$4) } getong delete SEMI from the three lines because there are SEMI in expr SEMI in stmt
  | ID M_UPDN LPAREN LITERAL RPAREN { M_updn($1,$2,$4) }
  | ID M_LEN LPAREN RPAREN          { M_len($1) }*/
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr SYNTHESIZE expr { Binop($1, Paral, $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | expr ASSIGN expr   { Assign($1, $3) }
  | expr CONCAT expr { Concat($1, $3) }
  /*| expr LPAREN actuals_opt RPAREN { Call($1, $3) } getong delete SEMI*/
  | METHOD LPAREN actuals_opt RPAREN { Method($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

/*actuals_rhy:
     nothing    { [] }
  | actuals_rhy_l      { List.rev $1 }

actuals_rhy_l:
     LITERAL                       { [$1] }
  | actuals_rhy_l COMMA LITERAL    { $3::$1 }*/
