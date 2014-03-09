
type literal = 
	  Int 	of int
	| Float of float
	| Bool 	of bool
	| Char 	of char
	| String of string
	| Stream of literal list;;

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
 	  Literal 				of literal
 	| Identifier 			of string 
 	| StreamAccess 			of string * int
 	| Application 			of string * expression list
 	| ScopedApplication 	of string * string * expression list 
 	| BinaryOperation 		of binary_operation * expression * expression
 	| UnaryOperation 		of unary_operation * expression
 	| VariableOperation 	of variable_operation * string
 	| Assignment 			of assignment * string * expression
 	| StreamConstruction 	of expression list;;

type test_type = 
	  Equality
	| NonEquality
	| LessThan
	| GreaterThan
	| LessThanOrEqual
	| GreaterThanOrEqual;;

type test = 
	  Test of test_type * expression * expression;;

type condition_type = 
	  LogicalAnd
	| LogicalOr;;

type condition = 
	  BinaryCondition of condition_type * condition * condition
	| UnaryCondition of test;;

type statement = 
	  Expression 	of expression
	| Skip 			of int * string
	| Output 		of expression
	| If 			of condition * statement list * statement list;;

type program = 
	  Program of string list * statement list * statement list;;
