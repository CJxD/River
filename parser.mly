%{

open Streams

%}

%token <int> INT
%token <float> FLOAT 
%token <bool> BOOL 
%token <char> CHAR
%token <string> IDENT
%token PLUS MINUS TIMES DIV LPAREN RPAREN POINTER LBRACKET RBRACKET ASSIGN ANGLELEFT ANGLERIGHT XOR AND OR COMMA INCREMENT DECREMENT LAMBDA IF THEN ELSE WHILE DO EQ GTE LTE NEQ
%token EOL
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV MODULO  /* medium precedence */
%left EXPONENTIAL 
%nonassoc UMINUS        /* highest precedence */
%start main             /* the entry point */
%type <int> main
%%

main:
   expr EOL                        { stream $1 }
;

expr:
     LBRACKET list RBRACKET		   { stream $2 }
   | IDENT ASSIGN expr  	       { let $1 = $3 in 0 }
   | stream POINTER INT            { index $1 $3 }
   | stream POINTER shift_op       { shift $1 $3 }
   | IF cond THEN expr ELSE expr   { if $2 then $4 else $6 }
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
	  INT { $1 }
	| FLOAT { $1 }
	| CHAR { $1 }
	| BOOL { $1 }
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
	  INT PLUS INT { $1 + $3 }
	| INT PLUS FLOAT { (float_of_int $1) +. $3 }
	| FLOAT PLUS INT { $1 +. (float_of_int $3) }
	| FLOAT PLUS FLOAT { $1 +. $3 } 

	| INT MINUS INT { $1 - $3 }
	| INT MINUS FLOAT { (float_of_int $1) -. $3 }
	| FLOAT MINUS INT { $1 -. (float_of_int $3) }
	| FLOAT MINUS FLOAT { $1 -. $3 }

	| INT TIMES INT { $1 * $3 }
	| INT TIMES FLOAT { (float_of_int $1) *. $3 }
	| FLOAT TIMES INT { $1 *. (float_of_int $3) }
	| FLOAT TIMES FLOAT { $1 *. $3 }

	| INT DIV INT { $1 / $3 }
	| INT DIV FLOAT { (float_of_int $1) /. $3 }
	| FLOAT DIV INT { $1 /. (float_of_int $3) }
	| FLOAT DIV FLOAT { $1 /. $3 }
;



