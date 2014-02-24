
(*
type condition = 
	  Equality of expression * expression
	| NonEquality of expression * expression
	| LessThan of expression * expression
	| GreaterThan of expression * expression
	| LessThanOrEqual of expression * expression
	| GreaterThanOrEqual of expression * expression;;

type operator =
	  Addition
	| Subtraction
	| Division
	| Multiplication;;

type ast = 
	  If of condition * expression * expression
	| While of condition * expression
	| Assignment of identifier * expression
	| Arithmetic of operator * expression * expression
	| Application of identifier * expression * expression 
	| Int of int
	| Float of float
	| Bool of bool
	| Char of char;;

*)

type literal = 
	Int of int
	| Float of float
	| Bool of bool
	| Char of char;;

type condition = 
	Equality of literal * literal;;

type expression =
	Assignment of string * literal;;

type statement = 
	  Expression of expression
	| If of condition * statement * statement;;

type statement_list = 
	  StatementList of statement * statement_list
	| Nothing;;

let outputLiteral = function
	  Int (n) 					-> "int(" ^ string_of_int n ^ ")"
	| Float (n) 				-> "float(" ^ string_of_float n ^ ")"
	| Bool (n) when n == true 	-> "bool(true)"
	| Bool (n) when n == false 	-> "bool(false)"
	| Char (n) 					-> "char(" ^ Char.escaped n ^ ")";;

let outputCondition = function 
	  Equality (l, r) -> "EqualityTest(" ^ outputLiteral l ^ ", " ^ outputLiteral r ^ ")";;

let outputExpression = function
	Assignment (i, v) -> "Assignment(" ^ i ^ ", " ^ outputLiteral v ^ ")";;

let rec outputStatement = function
	  Expression (e) 	-> outputExpression e
	| If (c, t, f) 		-> 
		"If(" ^ outputCondition c ^ ", " ^ outputStatement t ^ 
		", " ^ outputStatement f ^ ")";;

let rec outputStatementList = function
	  StatementList (s, r) 	-> outputStatement s ^ "\n" ^ outputStatementList r
	| Nothing 				-> "\n";;



	