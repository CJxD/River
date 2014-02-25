
open Language

let assign identifier literal =
	print_endline ("assigning " ^ (getLiteral literal) ^ " to variable " ^ identifier);;

let apply identifier expression =
	print_endline ("applying expression " ^ (getExpression expression) ^ " as argument to function " ^ identifier);;

let run_expression = function  
	| Literal (_) -> ()
	| VariableRead (_) -> ()
	| Assignment (Variable (identifier), literal) -> assign identifier literal
	| Application (Function (identifier), expression) -> apply identifier expression
	| _ -> ();;

let run_statement = function
	| Expression (expression) -> run_expression expression
	| If (_, _, _) -> ()
	| While (_, _) -> ();;

let rec run = function
	| Nothing 								-> ()
	| StatementList (statement, Nothing) 	-> run_statement statement
	| StatementList (statement, rest) 		-> run_statement statement; run rest;;
