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
	| statement statement_list 	{ StatementList ($1, $2) }
;
	
statement: 
	  expression EOL 										{ Expression $1 }
	| IF condition THEN statement_list ELSE statement_list 	{ If ($2, $4, $6) }
	| WHILE condition DO statement_list 					{ While ($2, $4) }
;

expression:
	  literal 							{ Literal $1 }
	| IDENT 							{ VariableRead (Variable $1) }
    | IDENT ASSIGN literal 				{ Assignment (Variable ($1), $3) }
    | IDENT LPAREN expression RPAREN 	{ Application (Function ($1), $3) }
;

condition:
	  expression EQ expression 			{ Equality ($1, $3) }
	| expression NEQ expression 		{ NonEquality ($1, $3) }
	| expression LBRACKET expression 	{ LessThan ($1, $3) }
	| expression RBRACKET expression 	{ GreaterThan ($1, $3) }
	| expression LTE expression 		{ LessThanOrEqual ($1, $3) }
	| expression GTE expression 		{ GreaterThanOrEqual ($1, $3) }

	 /* expression AND expression? / OR? */
;

literal:
	  INT 		{ Int $1 }
	| FLOAT 	{ Float $1 }
	| CHAR 		{ Char $1 }
	| BOOL 		{ Bool $1 }
;




