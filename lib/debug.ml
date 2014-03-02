
open Language

(* Print out a stream list list (strings) *)

let rec getIntList = function
	| (i :: rest) -> string_of_int i ^ " " ^ getIntList rest
	| [] -> "";;

let printIntList intlist =
	print_endline (getIntList intlist);;

let rec printInput = function
	| (value :: rest) -> 
		printIntList value;
		printInput rest
	| [] -> ();; 

(* Print out an AST *)

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
	| Group (e) 			-> "Expression(" ^ getExpression e ^ ")"
	| Math (o) 				-> getMath o
	| StreamAccess (s, i) 	-> "StreamAccess(" ^ getIdentifier s ^ ", " ^ string_of_int i ^ ")"
and getMath = function
	| Plus (l, r) 			-> "Plus(" ^ getExpression l ^ ", " ^ getExpression r ^ ")"
	| Minus (l, r) 			-> "Minus(" ^ getExpression l ^ ", " ^ getExpression r ^ ")"
	| Times (l, r) 			-> "Times(" ^ getExpression l ^ ", " ^ getExpression r ^ ")"
	| Divide (l, r) 		-> "Divide(" ^ getExpression l ^ ", " ^ getExpression r ^ ")"
	| Modulo (l, r) 		-> "Modulo(" ^ getExpression l ^ ", " ^ getExpression r ^ ")"
	| Power (l, r) 			-> "Power(" ^ getExpression l ^ ", " ^ getExpression r ^ ")"
	| UnaryMinus (e) 		-> "Negative(" ^ getExpression e ^ ")";;


let getCondition = function 
	  Equality (l, r) 			-> "EqualityTest(" ^ getExpression l ^ ", " ^ getExpression r ^ ")"
  	| NonEquality (l, r) 		-> "NonEqualityTest(" ^ getExpression l ^ ", " ^ getExpression r ^ ")"
  	| LessThan (l, r)		 	-> "LessThanTest(" ^ getExpression l ^ ", " ^ getExpression r ^ ")"
  	| GreaterThan (l, r) 		-> "GreaterThanTest(" ^ getExpression l ^ ", " ^ getExpression r ^ ")"
  	| LessThanOrEqual (l, r) 	-> "LessThanOrEqualTest(" ^ getExpression l ^ ", " ^ getExpression r ^ ")"
  	| GreaterThanOrEqual (l, r) -> "GreaterThanOrEqualTest(" ^ getExpression l ^ ", " ^ getExpression r ^ ")";;

let rec getStatement = function
	  Expression (e) 	-> getExpression e
	| Skip (i, s) 		-> "Skip(" ^ string_of_int i ^ ", " ^ getIdentifier s ^ ")"
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

let rec debugAssocList = function
	| (identifier, value) :: rest -> 
		print_endline (identifier ^ " = " ^ getIntList value ^ "; ");
		debugAssocList rest
	| [] -> ();;