
open Language 

let _ = 
	try
		let sourcefile = open_in Sys.argv.(1) in
			let lexbuf = Lexing.from_channel sourcefile in  
				let result = Parser.main Lexer.token lexbuf in

					outputStatementList result; 
					print_newline();
					flush stdout

	with 
		Parsing.Parse_error -> 
			print_string "There was a problem parsing the SDL program. Please check your syntax. \n"; 
			flush stdout
