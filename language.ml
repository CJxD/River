
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

type statement_list = 
	  StatementList of statement * statement_list
	| Nothing
and statement = 
	  Expression of expression
	| If of condition * statement_list * statement_list
	| While of condition * statement_list;;

let getLiteral = function
	  Int (n) 					-> "int(" ^ string_of_int n ^ ")"
	| Float (n) 				-> "float(" ^ string_of_float n ^ ")"
	| Bool (n) when n == true 	-> "bool(true)"
	| Bool (n) when n == false 	-> "bool(false)"
	| Char (n) 					-> "char(" ^ Char.escaped n ^ ")";;

let getCondition = function 
	  Equality (l, r) -> "EqualityTest(" ^ getLiteral l ^ ", " ^ getLiteral r ^ ")";;

let getExpression = function
	Assignment (i, v) -> "Assign(" ^ i ^ ", " ^ getLiteral v ^ ")";;

let rec getStatement = function
	  Expression (e) 	-> getExpression e
	| While (c, e)		-> "While(" ^ getCondition c ^ ", " ^ getStatementList e ^ ")"
	| If (c, t, f) 		-> 
		"If(" ^ getCondition c ^ ", " ^ getStatementList t ^ 
		", " ^ getStatementList f ^ ")"
and getStatementList = function
	  StatementList (s, r) 	-> "[" ^ getStatement s ^ ", " ^ getStatementList r ^ "]"
	| Nothing 				-> "";;



	