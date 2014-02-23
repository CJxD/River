
open Language 

let _ = 
	try
		let sourcefile = open_in Sys.argv.(0) in
			let lexbuf = Lexing.from_channel stdin in  
				let result = Sdlparser.main Sdllexer.main lexbuf in

					outputAst result; 
					print_newline();
					flush stdout

	with 
		Parsing.Parse_error -> 
			print_string "There was a problem parsing the SDL program. Please check your syntax. \n"; 
			flush stdout
