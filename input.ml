
open Str

exception Input_format_error of string;;

let parse channel =
	try
		let num_streams = int_of_string (input_line channel) in
		let stream_length = int_of_string (input_line channel) in	
		let streams = ref [] in
			try
			 	while true do 
			 		streams := Str.split (Str.regexp " ") (input_line channel) :: !streams
			 	done;
			 	[]
			with
				| End_of_file -> 
					if List.length !streams == num_streams then
						List.rev !streams
					else
						raise (Input_format_error "Stream count does not match the declared number")
	with
		| End_of_file 	-> raise (Input_format_error "Not enough data in input, only one line defined")
		| Failure _		-> raise (Input_format_error "Invalid integer for number of streams or stream length");;

let rec printStringList = function
	| (str :: rest) -> print_string (str ^ " "); printStringList rest
	| [] -> print_endline "";;

let rec checkInput = function
	| (value :: rest) -> 
		printStringList value;
		checkInput rest
	| [] -> ();; 