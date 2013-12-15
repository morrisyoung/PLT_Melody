%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACKETT LBRACKETB LBRACKETR RBRACKET LBRACE RBRACE LABRACKET RABRACKET COMMA
%token PLUS TIMES ASSIGN SYNTHESIZE CONCAT
%token EQ NEQ AND OR LT LEQ GT GEQ
%token IF ELSE FOR WHILE RETURN
%token FUNCTION NULL
%token <string> MAIN
%token <string> TYPE
/*%token <string> INSTRU*/
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
   FUNCTION all_type func_name LPAREN formals_opt RPAREN LBRACE func_bodys RBRACE
     { { rtype= $2;
         fname = $3;
   formals = $5;
   fbodys = $8;} }

all_type:
     TYPE  { $1 }
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
     all_type ID   {{p_type=$1;p_name=$2}}

func_bodys:
   /* nothing */    { [],[] }
 | func_bodys var_decl { List.rev ($2 :: List.rev (fst $1)), snd $1 }
 | func_bodys stmt { fst $1, List.rev ($2 :: List.rev (snd $1)) }

var_decl:
     TYPE ID SEMI                            {{ v_type=$1;v_name=$2;v_attr=("",0,0,0,0)}}
  | TRACK LABRACKET ID COMMA LITERAL COMMA LITERAL COMMA LITERAL COMMA LITERAL RABRACKET ID SEMI
						{{ v_type=$1;v_name=$13;v_attr=($3,$5,$7,$9,$11)}}
  | TRACK LABRACKET RABRACKET ID SEMI
						{{ v_type=$1;v_name=$4;v_attr=("",0,0,0,0)}}


stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
/*  | BREAK SEMI  { Break }  */
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

  LPAREN expr SEMI LITERAL RPAREN { Note_value($2,$4) } 
  | LBRACKETT actuals_opt RBRACKET       { Track_value($2)} /*getong changed, solve 1 r/r conflict, bring 1 s/r, not differentiate bar or rhythm right here*/  
  | LBRACKETB actuals_opt RBRACKET       { Bar_value1($2)} /*getong changed, solve 1 r/r conflict, bring 1 s/r, not differentiate bar or rhythm right here*/  
  | LBRACKETR actuals_opt RBRACKET       { Rhythm_value($2)} /*getong changed, solve 1 r/r conflict, bring 1 s/r, not differentiate bar or rhythm right here*/  
  
  | LBRACKETB expr SEMI LPAREN actuals_opt RPAREN RBRACKET { Bar_value2($2,$5) }
  | LITERAL          { Literal($1) }
  | PITCH_VALUE       { Pitch_value($1) }
  | STR              { Str($1) }
  | BOOL_VALUE       { Bool($1) }
  | NULL             { Null($1) }
  | ID               { Id($1) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3)}
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
  | ID ASSIGN expr   { Assign($1, $3) }
  | expr CONCAT expr { Concat($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
