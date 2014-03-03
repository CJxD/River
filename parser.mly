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
%token LBRACKET RBRACKET CURRENT
%token EQ NEQ LTE GTE LT GT
%token IF THEN ELSE ENDIF
%token USING BEGIN LOOP SKIP IN OUT
%token ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN INCREMENT DECREMENT

%token XOR AND OR NOT COMMA 

%right PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN
%right ASSIGN
%left EQ NEQ
%left GT GTE LT LTE
%left PLUS MINUS 
%left TIMES DIVIDE MODULO
%right UMINUS
%right PREFIXINCREMENT PREFIXDECREMENT
%left POSTFIXINCREMENT POSTFIXDECREMENT

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
	  expression EOL 												{ Expression $1 }
	| SKIP EOL														{ Skip (1, "") }
	| SKIP IN IDENT EOL 											{ Skip (1, $3) }
	| SKIP INT EOL													{ Skip ($2, "") }  
	| SKIP INT IN IDENT EOL 										{ Skip ($2, $4) }
	| OUT expression EOL 											{ Output $2 }
	| IF condition THEN statement_list ELSE statement_list ENDIF 	{ If ($2, $4, $6) }
	| IF condition THEN statement_list ENDIF 						{ If ($2, $4, EndStatement) }
	| IF condition THEN statement 									{ If ($2, StatementList ($4, EndStatement), EndStatement) }
	| IF condition THEN statement ELSE statement 					{ If ($2, StatementList ($4, EndStatement), StatementList ($6, EndStatement)) }
;

expression:
	  literal 						{ Literal $1 }
	| assignment 					{ $1 }
	| math 							{ $1 }
	| variable_operation 			{ $1 }
	| IDENT 						{ Identifier $1 }
	| LPAREN expression RPAREN		{ Group $2 }
	| IDENT LBRACKET INT RBRACKET 	{ StreamAccess ($1, $3) }
	| IDENT CURRENT 				{ StreamAccess ($1, 0) }
	| IDENT shift_list 				{ StreamAccess ($1, $2) }
;

shift_list:
	  GT 			{ 1 }
	| shift_list GT { $1 + 1 }
;

assignment:
	  IDENT ASSIGN expression 		{ Assignment (StandardAssign, $1, $3) }
	| IDENT PLUSASSIGN expression 	{ Assignment (PlusAssign, $1, $3) }	
	| IDENT MINUSASSIGN expression 	{ Assignment (MinusAssign, $1, $3) }	
	| IDENT TIMESASSIGN expression 	{ Assignment (TimesAssign, $1, $3) }	
	| IDENT DIVIDEASSIGN expression { Assignment (DivideAssign, $1, $3) }	
;

math:
	  expression PLUS expression 	{ BinaryOperation (Plus, $1, $3)  }
	| expression MINUS expression 	{ BinaryOperation (Minus, $1, $3) }
	| expression TIMES expression 	{ BinaryOperation (Times, $1, $3) }
	| expression DIVIDE expression 	{ BinaryOperation (Divide, $1, $3) }
	| expression MODULO expression 	{ BinaryOperation (Modulo, $1, $3) }
	| expression POWER expression 	{ BinaryOperation (Power, $1, $3) }
	| MINUS expression %prec UMINUS { UnaryOperation (UnaryMinus, $2) }
;

variable_operation:
	  IDENT INCREMENT %prec POSTFIXINCREMENT 	{ VariableOperation (PostfixIncrement, $1) }
	| IDENT DECREMENT %prec POSTFIXDECREMENT 	{ VariableOperation (PostfixDecrement, $1) }
	| INCREMENT IDENT %prec PREFIXINCREMENT 	{ VariableOperation (PrefixIncrement, $2) }
	| DECREMENT IDENT %prec PREFIXDECREMENT 	{ VariableOperation (PrefixDecrement, $2) }
;

condition:
	  expression EQ expression 	{ Condition (Equality, $1, $3) }
	| expression NEQ expression { Condition (NonEquality, $1, $3) }
	| expression LT expression 	{ Condition (LessThan, $1, $3) }
	| expression GT expression 	{ Condition (GreaterThan, $1, $3) }
	| expression LTE expression { Condition (LessThanOrEqual, $1, $3) }
	| expression GTE expression { Condition (GreaterThanOrEqual, $1, $3) }

	 /* expression AND expression? / OR? */
;

literal:
	  INT 	{ Int $1 }
	| FLOAT { Float $1 }
	| CHAR 	{ Char $1 }
	| BOOL 	{ Bool $1 }
;