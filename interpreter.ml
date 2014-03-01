
open Language

let run_expression = function  
	| Literal (literal) 			-> getLiteral literal
	| StreamAccess (stream, index) 	-> "Accessing element " ^ string_of_int index ^ " of stream " ^ stream;;

let run_skip elements stream =
	print_endline ("Skipping " ^ string_of_int elements ^ " in stream '" ^ stream ^ "'");;

let run_output expression = 
	print_endline ("Outputting stream element: " ^ run_expression expression);;

let run_statement = function
	| Expression (expression) 	-> print_endline (run_expression expression)
	| Skip (elements, stream) 	-> run_skip elements stream
	| Output (expression) 		-> run_output expression
	| If (_, _, _) 				-> ();;

let rec run_statement_list = function
	| StatementList (statement, EndStatement) 	-> run_statement statement
	| StatementList (statement, rest) 			-> run_statement statement; run_statement_list rest
	| EndStatement 								-> ();;

let load_stream identifier = 
	print_endline ("Loading stream " ^ identifier);;

let rec define_streams = function 
	| IdentifierList (identifier, EndIdentifier) 	-> load_stream identifier
	| IdentifierList (identifier, rest) 			-> load_stream identifier; define_streams rest
	| EndIdentifier 								-> ();;

let run = function
	| Program (using, start, loop) -> 
		define_streams using;
		run_statement_list start;
		run_statement_list loop;;
