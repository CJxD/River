%{

open Language
open Errors

%}

%token <int> INT
%token <float> FLOAT 
%token <bool> BOOL 
%token <char> CHAR
%token <string> IDENT
%token <string> STRING

%token EOF EOL
%token TRUE FALSE
%token PLUS MINUS TIMES DIVIDE MODULO POWER
%token LPAREN RPAREN
%token LBRACKET RBRACKET CURRENT
%token EQ NEQ LTE GTE LT GT
%token IF THEN ELSE ENDIF
%token USING BEGIN LOOP SKIP IN OUT
%token AND OR
%token ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN INCREMENT DECREMENT
%token COMMA

%right PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN
%right ASSIGN
%left OR
%left AND 
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
	| USING identifier_list LOOP statement_list EOF 						{ Program ($2, [], $4) }
	| USING identifier_list BEGIN statement_list EOF 						{ Program ($2, $4, []) }
	| error { 
			parse_err "Malformed program structure, 'with' is required, 'begin' and 'loop' are optional but must have statements."; 
			Program ([], [], []) 
		}
;

identifier_list:
	  IDENT 						{ [ $1 ] }
	| IDENT COMMA identifier_list 	{ $1 :: $3 }
	| error { 
			parse_err "Your 'with' list is malformed. Expected another list element here."; 
			[] 
		}
;

statement_list:
	  statement 				{ [ $1 ] }
	| statement statement_list 	{ $1 :: $2 }
	| error { 
			parse_err "Not expecting program block here. Statement list expected after begin or loop."; 
			[] 
		}
;

statement: 
	  expression EOL 												{ Expression $1 }
	| SKIP EOL														{ Skip (1, "") }
	| SKIP IN IDENT EOL 											{ Skip (1, $3) }
	| SKIP INT EOL													{ Skip ($2, "") }  
	| SKIP INT IN IDENT EOL 										{ Skip ($2, $4) }
	| OUT expression EOL 											{ Output $2 }
	| IF condition THEN statement_list ELSE statement_list ENDIF 	{ If ($2, $4, $6) }
	| IF condition THEN statement_list ENDIF 						{ If ($2, $4, []) }
	| error { 
			parse_err "This statement is malformed."; 
			Expression (Literal (Int 0)) 
		}
;

expression_list:
	  expression 						{ [ $1 ] }
	| expression COMMA expression_list 	{ $1 :: $3 }
	| error { 
			parse_err "Not expecting left parenthesis here. Expecting expression after comma."; 
			[] 
		}
;

expression:
	  literal 								{ Literal $1 }
	| assignment 							{ $1 }
	| math 									{ $1 }
	| variable_operation 					{ $1 }
	| IDENT LPAREN expression_list RPAREN 	{ Application ($1, $3) }
	| IDENT LPAREN RPAREN 					{ Application ($1, []) }
	| IDENT 								{ Identifier $1 }
	| LPAREN expression RPAREN				{ Group $2 }
	| IDENT LBRACKET INT RBRACKET 			{ StreamAccess ($1, $3) }
	| IDENT CURRENT 						{ StreamAccess ($1, 0) }
	| IDENT shift_list 						{ StreamAccess ($1, $2) }
	| LBRACKET expression_list RBRACKET 	{ StreamConstruction $2 }
	| LBRACKET RBRACKET 					{ StreamConstruction [] }
	| error { 
			parse_err "This expression is malformed."; 
			Literal (Int 0) 
		}
;

shift_list:
	  GT 			{ 1 }
	| shift_list GT { $1 + 1 }
	| error { 
			parse_err "Invalid shift operation. Only expecting > here."; 
			0 
		}
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
	  test 						{ UnaryCondition $1 }
	| condition AND condition 	{ BinaryCondition (LogicalAnd, $1, $3) }
	| condition OR condition 	{ BinaryCondition (LogicalOr, $1, $3) }
;

test:
	  expression EQ expression 	{ Test (Equality, $1, $3) }
	| expression NEQ expression { Test (NonEquality, $1, $3) }
	| expression LT expression 	{ Test (LessThan, $1, $3) }
	| expression GT expression 	{ Test (GreaterThan, $1, $3) }
	| expression LTE expression { Test (LessThanOrEqual, $1, $3) }
	| expression GTE expression { Test (GreaterThanOrEqual, $1, $3) }
	| error { 
			parse_err "This test is malformed."; 
			Test (Equality, Literal (Int 0), Literal (Int 0)) 
		}
;

literal:
	  INT 		{ Int $1 }
	| FLOAT 	{ Float $1 }
	| CHAR 		{ Char $1 }
	| BOOL 		{ Bool $1 }
	| STRING 	{ String $1 }
;