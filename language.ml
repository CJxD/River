
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

type identifier = 
	  Variable of string
	| Function of string;;

type expression =
 	  Literal of literal
 	| VariableRead of identifier
	| Assignment of identifier * literal
	| Application of identifier * expression;;

type unary_operation =
	  PostfixIncrement of identifier
	| PostfixDecrement of identifier
	| PrefixIncrement of identifier
	| PrefixDecrement of identifier;; 

type condition = 
	  Equality of expression * expression
	| NonEquality of expression * expression
	| LessThan of expression * expression
	| GreaterThan of expression * expression
	| LessThanOrEqual of expression * expression
	| GreaterThanOrEqual of expression * expression;;

type statement_list = 
	  StatementList of statement * statement_list
	| Nothing
and statement = 
	  Expression of expression
	| If of condition * statement_list * statement_list
	| While of condition * statement_list;;

let getLiteral = function
	  Int (n) 		-> "int(" ^ string_of_int n ^ ")"
	| Float (n) 	-> "float(" ^ string_of_float n ^ ")"
	| Bool (true) 	-> "bool(true)"
	| Bool (false) 	-> "bool(false)"
	| Char (n) 		-> "char(" ^ Char.escaped n ^ ")";;

let getIdentifier = function
	  Variable (n) -> "Variable(" ^ n ^ ")"
	| Function (n) -> "Function(" ^ n ^ ")";;

let rec getExpression = function
	  Literal (v) 			-> getLiteral v
	| VariableRead (i) 		-> "ReadVariable(" ^ getIdentifier i ^ ")"
	| Assignment (i, v) 	-> "Assign(" ^ getIdentifier i ^ ", " ^ getLiteral v ^ ")"
	| Application (i, a) 	-> "Application(" ^ getIdentifier i ^ ", " ^ getExpression a ^ ")";; 

let getCondition = function 
	  Equality (l, r) 			-> "EqualityTest(" ^ getExpression l ^ ", " ^ getExpression r ^ ")"
  	| NonEquality (l, r) 		-> "NonEqualityTest(" ^ getExpression l ^ ", " ^ getExpression r ^ ")"
  	| LessThan (l, r)		 	-> "LessThanTest(" ^ getExpression l ^ ", " ^ getExpression r ^ ")"
  	| GreaterThan (l, r) 		-> "GreaterThanTest(" ^ getExpression l ^ ", " ^ getExpression r ^ ")"
  	| LessThanOrEqual (l, r) 	-> "LessThanOrEqualTest(" ^ getExpression l ^ ", " ^ getExpression r ^ ")"
  	| GreaterThanOrEqual (l, r) -> "GreaterThanOrEqualTest(" ^ getExpression l ^ ", " ^ getExpression r ^ ")";;

let rec getStatement = function
	  Expression (e) 	-> getExpression e
	| While (c, e)		-> "While(" ^ getCondition c ^ ", " ^ getStatementList e ^ ")"
	| If (c, t, f) 		-> "If(" ^ getCondition c ^ ", " ^ getStatementList t ^ ", " ^ getStatementList f ^ ")"
and getStatementList = function
	  StatementList (s, r) 	-> "[" ^ getStatement s ^ ", " ^ getStatementList r ^ "]"
	| Nothing 				-> "";;



	