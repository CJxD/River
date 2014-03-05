
type literal = 
	  Int 	of int
	| Float of float
	| Bool 	of bool
	| Char 	of char
	| String of string;;

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
 	| Application 		of string * expression list
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
	| GreaterThanOrEqual
	| LogicalAnd
	| LogicalOr;;

type condition = 
	 Condition of test * expression * expression;;

type statement = 
	  Expression 	of expression
	| Skip 			of int * string
	| Output 		of expression
	| If 			of condition * statement list * statement list;;

type program = 
	  Program of string list * statement list * statement list;;
