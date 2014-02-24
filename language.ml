
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

type expression =
	Assignment of string * literal;;

type statement = 
	  Statement of expression
	| BlankLine;; (* for blank lines *)

type statement_list = 
	  StatementList of statement * statement_list
	| Nothing;;

let outputLiteral = function
	Int (n) -> print_int n
	| Float (n) -> print_float n
	| Bool (n) when n == true -> print_string "bool(true)"
	| Bool (n) when n == false -> print_string "bool(false)"
	| Char (n) -> print_char n;;

let outputExpression = function
	Assignment (identifier, value) -> 
		print_string identifier; 
		print_string " = ";
		outputLiteral value;
		print_newline();;

let outputStatement = function
	  Statement (expression) -> outputExpression expression
	| BlankLine -> ();;

let rec outputStatementList = function
	  StatementList (statement, rest) -> 
	  	outputStatement statement; 
	  	outputStatementList rest
	| Nothing -> print_newline;;



	