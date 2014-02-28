
(* AST types *)

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
 	| StreamAccess 	of string * int;;

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

(* Print out the AST for debugging purposes *)

let getLiteral = function
	  Int (n) 		-> "int(" ^ string_of_int n ^ ")"
	| Float (n) 	-> "float(" ^ string_of_float n ^ ")"
	| Bool (true) 	-> "bool(true)"
	| Bool (false) 	-> "bool(false)"
	| Char (n) 		-> "char(" ^ Char.escaped n ^ ")";;

let getIdentifier = function
	  i -> "identifier(" ^ i ^ ")";;

let rec getExpression = function
	  Literal (v) 			-> getLiteral v
	| StreamAccess (s, i) 	-> "StreamAccess(" ^ getIdentifier s ^ ", " ^ string_of_int i ^ ")";;

let getCondition = function 
	  Equality (l, r) 			-> "EqualityTest(" ^ getExpression l ^ ", " ^ getExpression r ^ ")"
  	| NonEquality (l, r) 		-> "NonEqualityTest(" ^ getExpression l ^ ", " ^ getExpression r ^ ")"
  	| LessThan (l, r)		 	-> "LessThanTest(" ^ getExpression l ^ ", " ^ getExpression r ^ ")"
  	| GreaterThan (l, r) 		-> "GreaterThanTest(" ^ getExpression l ^ ", " ^ getExpression r ^ ")"
  	| LessThanOrEqual (l, r) 	-> "LessThanOrEqualTest(" ^ getExpression l ^ ", " ^ getExpression r ^ ")"
  	| GreaterThanOrEqual (l, r) -> "GreaterThanOrEqualTest(" ^ getExpression l ^ ", " ^ getExpression r ^ ")";;

let rec getStatement = function
	  Expression (e) 	-> getExpression e
	| Skip (i, s) 		-> "Skip(" ^ string_of_int i ^ ", '" ^ getIdentifier s ^ "')"
	| Output (e) 		-> "Output(" ^ getExpression e ^ ")"
	| If (c, t, f) 		-> "If(" ^ getCondition c ^ ", " ^ getStatementList t ^ ", " ^ getStatementList f ^ ")"
and getStatementList = function
	  StatementList (s, r) 	-> getStatement s ^ ", " ^ getStatementList r
	| EndStatement 			-> "";;

let rec getIdentifierList = function
	  IdentifierList (i, r) -> getIdentifier i ^ ", " ^ getIdentifierList r
	| EndIdentifier 		-> "";;

let getProgram = function
	  Program (s, b, l) -> "Program([" ^ getIdentifierList s ^ "], [" ^ getStatementList b ^ "], [" ^ getStatementList l ^ "])";;


	