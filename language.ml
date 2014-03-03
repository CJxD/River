
exception Fatal of string;;
exception Warning of string;;
exception End_of_stream;;
exception Undeclared_identifier of string;;

type literal = 
	  Int 	of int
	| Float of float
	| Bool 	of bool
	| Char 	of char;;

type identifier_list = 
	  IdentifierList of string * identifier_list
	| EndIdentifier;;

type assignment = 
	  StandardAssign
	| PlusAssign
	| MinusAssign
	| TimesAssign
	| DivideAssign;;

type binary_operation = 
	  Plus
 	| Minus
 	| Divide
 	| Times
 	| Modulo
 	| Power;;

type unary_operation = 
	  UnaryMinus;;

type variable_operation = 
	  PostfixIncrement
	| PostfixDecrement
	| PrefixIncrement
	| PrefixDecrement;;

type expression =
 	  Literal 			of literal
 	| Identifier 		of string 
 	| StreamAccess 		of string * int
 	| BinaryOperation 	of binary_operation * expression * expression
 	| UnaryOperation 	of unary_operation * expression
 	| VariableOperation of variable_operation * string
 	| Group 			of expression
 	| Assignment 		of assignment * string * expression;;

type test = 
	  Equality
	| NonEquality
	| LessThan
	| GreaterThan
	| LessThanOrEqual
	| GreaterThanOrEqual;;

type condition = 
	 Condition of test * expression * expression;;

type statement_list = 
	  StatementList of statement * statement_list
	| EndStatement
and statement = 
	  Expression 	of expression
	| Skip 			of int * string
	| Output 		of expression
	| If 			of condition * statement_list * statement_list;;

type program = 
	  Program of identifier_list * statement_list * statement_list;;
