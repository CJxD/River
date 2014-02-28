
open Language 
open Interpreter

let _ = 
	try
		let sourcefile = Sys.argv.(1) in
			try
				let lexbuf = Lexing.from_channel (open_in sourcefile) in  
				let program = Parser.main Lexer.token lexbuf in

					if Array.length Sys.argv == 3 then
						match Sys.argv.(2) with
							| "-printast" -> 
								print_string (getProgram program);
								print_newline();
								flush stdout
							| _ -> 
								raise (Invalid_argument ("Unrecognized debugging setting: " ^ Sys.argv.(2)))
					else
						Interpreter.run program
			with 
				| Invalid_argument e ->
					print_endline ("Invalid argument: " ^ e)
				| Sys_error e -> 
					print_endline ("Could not read source file: " ^ e)
				| Parsing.Parse_error -> 
					print_endline "Source file could not be parsed (Syntax Error)."
	with
		Invalid_argument e -> 
			print_endline "No input file specified.\nUsage: river <source file> <printast>"

