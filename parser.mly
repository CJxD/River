%{

open Language

%}

%token <int> INT
%token <float> FLOAT 
%token <bool> BOOL 
%token <char> CHAR
%token <string> IDENT

%token EOF EOL
%token TRUE FALSE
%token PLUS MINUS TIMES DIVIDE MODULO POWER
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token EQ NEQ LTE GTE LT GT
%token IF THEN ELSE
%token USING BEGIN LOOP SKIP IN OUT
%token ASSIGN

%token XOR AND OR COMMA INCREMENT DECREMENT

%right ASSIGN
%left EQ NEQ
%left GT GTE LT LTE
%left PLUS MINUS 
%left TIMES DIVIDE MODULO
%nonassoc UMINUS

%start main
%type <Language.program> main

%%

main:
	  USING identifier_list BEGIN statement_list LOOP statement_list EOF 	{ Program ($2, $4, $6) }
	| USING identifier_list LOOP statement_list EOF 						{ Program ($2, EndStatement, $4) }
;

identifier_list:
	  IDENT 						{ IdentifierList ($1, EndIdentifier) }
	| IDENT COMMA identifier_list 	{ IdentifierList ($1, $3) }
;

statement_list:
	  statement 				{ StatementList ($1, EndStatement) }
	| statement statement_list 	{ StatementList ($1, $2) }
;

statement: 
	  expression EOL 										{ Expression $1 }
	| SKIP EOL												{ Skip (1, "") }
	| SKIP IN IDENT EOL 									{ Skip (1, $3) }
	| SKIP INT EOL											{ Skip ($2, "") }  
	| SKIP INT IN IDENT EOL 								{ Skip ($2, $4) }
	| OUT expression EOL 									{ Output $2 }
	| IF condition THEN statement_list ELSE statement_list 	{ If ($2, $4, $6) }
;

expression:
	  literal 						{ Literal $1 }
	| IDENT ASSIGN expression 		{ Assignment ($1, $3) }
	| IDENT 						{ Identifier $1 }
	| math 							{ Math $1  }
	| LPAREN expression RPAREN		{ Group $2 }
	| IDENT LBRACKET INT RBRACKET 	{ StreamAccess ($1, $3) }
;

math:
	  expression PLUS expression 	{ Plus ($1, $3)  }
	| expression MINUS expression 	{ Minus ($1, $3) }
	| expression TIMES expression 	{ Times ($1, $3) }
	| expression DIVIDE expression 	{ Divide ($1, $3) } 
	| expression MODULO expression 	{ Modulo ($1, $3) } 
	| expression POWER expression 	{ Power ($1, $3) }
	| MINUS expression %prec UMINUS { UnaryMinus $2 }
;

condition:
	  expression EQ expression 	{ Equality ($1, $3) }
	| expression NEQ expression { NonEquality ($1, $3) }
	| expression LT expression 	{ LessThan ($1, $3) }
	| expression GT expression 	{ GreaterThan ($1, $3) }
	| expression LTE expression { LessThanOrEqual ($1, $3) }
	| expression GTE expression { GreaterThanOrEqual ($1, $3) }

	 /* expression AND expression? / OR? */
;

literal:
	  INT 	{ Int $1 }
	| FLOAT { Float $1 }
	| CHAR 	{ Char $1 }
	| BOOL 	{ Bool $1 }
;