
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


let rec outputAst = function
	If