
open Language 

let _ = 
	let sourcefile = open_in Sys.argv.(1) in
		try 
			let lexbuf = Lexing.from_channel sourcefile in  
			let ast = Parser.main Lexer.token lexbuf in
				print_string "Parsed source file ";
				print_string Sys.argv.(1);
				print_newline();
				print_newline();
				outputStatementList ast; 
				print_newline();
				flush stdout
		with 
			Parsing.Parse_error -> 
			print_string "Parse error \n"; 
			flush stdout 
