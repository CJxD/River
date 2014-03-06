
open Str
open Language

exception Input_format_error of string;;

(* Ocaml 3.12 doesn't have String.trim *)
let string_trim str =
	if str = "" then "" else
	let search_pos init p next =
		let rec search i =
			if p i then raise(Failure "empty") else
			match str.[i] with
			| ' ' | '\n' | '\r' | '\t' -> search (next i)
			| _ -> i
		in
		search init
	in
	let len = String.length str in
	try
		let left = search_pos 0 (fun i -> i >= len) (succ)
		and right = search_pos (len - 1) (fun i -> i < 0) (pred)
		in
		String.sub str left (right - left + 1)   with   | Failure "empty" -> "" ;;

let string_of_char str =
	if String.length str = 1
	then
		String.get str 0
	else
		raise (Failure "character not in correct format - expected single character")

let string_of_quoted_char str =
	if String.get str 0 = '\''
		&& String.get str ((String.length str) - 1) = '\''
		then
			String.get str 1
		else
			raise (Failure "character not in correct format - expected 'char'")

let string_of_quoted_string str =
	if String.get str 0 = '"'
		&& String.get str ((String.length str) - 1) = '"'
		then
			String.sub str 1 ((String.length str) - 2)
		else 
			raise (Failure "string not in correct format - expected \"char\"")

let literal_of_string str =
	try
		Int (int_of_string str)
	with Failure bad_format ->
	try
		Float (float_of_string str)
	with Failure bad_format ->
	try
		Bool (bool_of_string str)
	with Invalid_argument invalid_argument ->
	try 
		Char (string_of_char str)
	with Failure bad_format ->
	try 
		Char (string_of_quoted_char str)
	with Failure bad_format ->
	try
		String (string_of_quoted_string str)
	with Failure bad_format ->
		raise (Failure ("Unable to parse " ^ str))
		
let parse channel =
	try

		(* Read the number & length declarations first *)

		let num_streams = int_of_string (string_trim (input_line channel)) in
		let stream_length = int_of_string (string_trim (input_line channel)) in	
		let streams = ref [] in
			try
			 	while true do 

			 		(* Get line, trim it, split it on space & convert to ints *)

			 		let trimmed = string_trim (input_line channel) in
			 		let split = Str.split (Str.regexp " ") trimmed in
			 		let stream = List.map literal_of_string split in

			 			if List.length stream == stream_length then 
			 				begin

			 					streams := stream :: !streams;

								(* if stream list is now declared size raise end_of_file *)

								if List.length !streams == num_streams then
			 						raise End_of_file
			 						
							end
			 			else
			 				raise (
			 					Input_format_error (
			 						"Length of stream " ^ 
			 						(string_of_int ((List.length !streams) + 1)) ^ 
			 						" (" ^ 
			 						(string_of_int (List.length stream)) ^ 
			 						") does not match the declared length (" ^
			 						(string_of_int stream_length) ^
			 						")"))
			 	done;
			 	[]
			with
				| Failure e ->
					raise (Input_format_error (
						"Stream " ^ 
						(string_of_int ((List.length !streams) + 1)) ^
						" contains an invalid element (non-integer)"))

				| End_of_file -> 

					(* Check number of streams read matches declaration *)

					let actual_num_streams = List.length !streams in
						if actual_num_streams == num_streams then
							List.rev !streams
						else
							raise (
								Input_format_error (
									"Stream count (" ^ 
									(string_of_int actual_num_streams) ^ 
									") does not match the declared count (" ^ 
									(string_of_int num_streams) ^ 
									")"))
	with
		| End_of_file -> 
			raise (Input_format_error 
				"Not enough data in input, only one line defined")

		| Failure _ -> 
			raise (Input_format_error 
				"Invalid integer for number of streams or stream length");;
