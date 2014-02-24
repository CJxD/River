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
%token TRUE FALSE EOF
%token EQ GTE LTE NEQ
%token EOL
%left PLUS MINUS		/* low precedence */
%left TIMES DIV MODULO  
%left EXPONENTIAL 
%nonassoc UMINUS        /* high precedence */
%start main
%type <Language.statement_list> main

%%

main:
	statement_list EOF { $1 }
;

statement_list:
	  statement 				{ StatementList ($1, Nothing) }
	| statement_list statement 	{ StatementList ($2, $1) }
;
	
statement: 
	  expr EOL { Statement $1 }
	| expr EOF { Statement $1 }
;



expr:
    IDENT ASSIGN literal { Assignment ($1, $3) }
;

literal:
	  INT 		{ Int $1 }
	| FLOAT 	{ Float $1 }
	| CHAR 		{ Char $1 }
	| BOOL 		{ Bool $1 }
;




