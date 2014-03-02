
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

type expression =
 	  Literal 		of literal
 	| Identifier 	of string 
 	| StreamAccess 	of string * int
 	| Math 			of math
 	| Group 		of expression
 	| Assignment 	of string * expression
 and math = 
	  Plus 			of expression * expression
 	| Minus 		of expression * expression
 	| Divide 		of expression * expression
 	| Times 		of expression * expression
 	| Modulo 		of expression * expression
 	| Power 		of expression * expression
 	| UnaryMinus 	of expression;;

type condition = 
	  Equality 				of expression * expression
	| NonEquality 			of expression * expression
	| LessThan 				of expression * expression
	| GreaterThan 			of expression * expression
	| LessThanOrEqual 		of expression * expression
	| GreaterThanOrEqual 	of expression * expression;;

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
