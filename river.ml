
open Language 
open Interpreter
open Input
open Errors

let _ = 
	try
		let sourcefile = Sys.argv.(1) in
			try
				let lexbuf = Lexing.from_channel (open_in sourcefile) in  
				let program = Parser.main Lexer.token lexbuf in
				let interpreter = new Interpreter.interpreter in

					interpreter#run program (Input.parse stdin);

					print_endline interpreter#get_output
			with 
				| Invalid_argument e ->
					print_endline ("Invalid argument: " ^ e)
				| Sys_error e -> 
					print_endline ("Could not read source file: " ^ e)
				| Parsing.Parse_error -> 
					print_endline "Syntax error: No additional information available."
				| Input.Input_format_error e ->
					print_endline ("Input format error: " ^ e)
				| Errors.Lexing_error e ->
					print_endline e
				| Errors.Parse_error e ->
					print_endline e
				| Errors.Fatal e ->
					print_endline ("Fatal error: " ^ e)
				| Errors.Undeclared_identifier i ->
					print_endline ("Use of undeclared identifiers is disallowed, you used: " ^ i)
	with
		Invalid_argument e -> 
			print_endline "No input file specified.\nUsage: river <source file>"

