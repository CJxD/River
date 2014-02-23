%{

open Language

%}

%token <int> INT
%token <float> FLOAT 
%token <bool> BOOL 
%token <char> CHAR
%token <string> IDENT
%token PLUS MINUS TIMES DIV LPAREN RPAREN 
%token POINTER LBRACKET RBRACKET ASSIGN ANGLELEFT ANGLERIGHT 
%token XOR AND OR COMMA INCREMENT DECREMENT LAMBDA IF THEN ELSE WHILE DO 
%token EQ GTE LTE NEQ
%token EOL
%left PLUS MINUS		/* low precedence */
%left TIMES DIV MODULO  
%left EXPONENTIAL 
%nonassoc UMINUS        /* high precedence */
%start main
%type <Language.ast> main

%%

main:
   expr EOL                        { stream $1 }
;

expr:
     LBRACKET list RBRACKET		   { stream $2 }
   | IDENT ASSIGN expr  	       { Assignment $1 $3 }
   | stream POINTER INT            { index $1 $3 }
   | stream POINTER shift_op       { shift $1 $3 }
   | IF cond THEN expr ELSE expr   { If $2 $4 $6 }
/*   | WHILE cond DO expr			   { while $2 do $4 done } */
   | arithmetic                    { $1 }
   | literal                       { $1 }
/*   | IDENT LPAREN expr RPAREN      { ($1) $3 } */
;

cond:
     expr EQ expr                 { $1 == $3 }
   | expr GTE expr                { $1 >= $3 }
   | expr LTE expr                { $1 <= $3 }
   | expr ANGLELEFT expr          { $1 < $3 }
   | expr ANGLERIGHT expr         { $1 > $3 }
   | expr NEQ expr                { $1 != $3 }
;

literal:
	  INT 		{ Int $1 }
	| FLOAT 	{ Float $1 }
	| CHAR 		{ Char $1 }
	| BOOL 		{ Bool $1 }
;

stream:
      list { $1 }
;

shift_op:
      ANGLERIGHT       { 1 }
    | ANGLERIGHT INT   { $2 }
;

list:
	  literal COMMA literal       { $1 :: $3 }
;

arithmetic:
	  INT PLUS INT 		{ Int ($1 + $3) }
	| INT PLUS FLOAT 	{ Float ((float_of_int $1) +. $3) }
	| FLOAT PLUS INT 	{ Float ($1 +. (float_of_int $3)) }
	| FLOAT PLUS FLOAT 	{ Float ($1 +. $3) } 

	| INT MINUS INT 	{ Int ($1 - $3) }
	| INT MINUS FLOAT 	{ Float ((float_of_int $1) -. $3) }
	| FLOAT MINUS INT 	{ Float ($1 -. (float_of_int $3)) }
	| FLOAT MINUS FLOAT { Float ($1 -. $3) }

	| INT TIMES INT 	{ Int ($1 * $3) }
	| INT TIMES FLOAT 	{ Float ((float_of_int $1) *. $3) }
	| FLOAT TIMES INT 	{ Float ($1 *. (float_of_int $3)) }
	| FLOAT TIMES FLOAT { Float ($1 *. $3) }

	| INT DIV INT 		{ Int ($1 / $3) }
	| INT DIV FLOAT 	{ Float ((float_of_int $1) /. $3) }
	| FLOAT DIV INT 	{ Float ($1 /. (float_of_int $3)) }
	| FLOAT DIV FLOAT 	{ Float ($1 /. $3) }
;



