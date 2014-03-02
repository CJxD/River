
open Language 
open Interpreter
open Input
open Debug

let _ = 
	try
		let sourcefile = Sys.argv.(1) in
			try
				let lexbuf = Lexing.from_channel (open_in sourcefile) in  
				let program = Parser.main Lexer.token lexbuf in
				let interpreter = new Interpreter.interpreter in

					if Array.length Sys.argv == 3 then
						match Sys.argv.(2) with
							| "-printast" -> 
								print_string (getProgram program);
								print_newline();
								flush stdout
							| "-ignoreinput" ->
								interpreter#run program []
							| "-input" ->
								Debug.printInput (Input.parse stdin)
							| _ -> 
								raise (Invalid_argument ("Unrecognized debugging setting: " ^ Sys.argv.(2)))
					else
						interpreter#run program (Input.parse stdin);
						print_endline interpreter#get_output
			with 
				| Invalid_argument e ->
					print_endline ("Invalid argument: " ^ e)
				| Sys_error e -> 
					print_endline ("Could not read source file: " ^ e)
				| Parsing.Parse_error -> 
					print_endline "Source file could not be parsed (Syntax Error)."
				| Input.Input_format_error e ->
					print_endline ("Input format error: " ^ e)
				| Language.Fatal e ->
					print_endline ("Fatal error: " ^ e)
	with
		Invalid_argument e -> 
			print_endline "No input file specified.\nUsage: river <source file> <printast>"

