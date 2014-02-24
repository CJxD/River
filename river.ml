
open Language 

let _ = 
	try
		let source = open_in Sys.argv.(1) in
		let lexbuf = Lexing.from_channel source in  
		let ast = Parser.main Lexer.token lexbuf in
			print_string (getStatementList ast); 
			print_newline();
			flush stdout;
	with 
		| Invalid_argument e ->
			print_endline "No input file specified.\nUsage: river <source file>"
		| Sys_error e -> 
			print_endline ("Could not read source file: " ^ e)
		| Parsing.Parse_error -> 
			print_endline "Source file could not be parsed (Syntax Error)."

