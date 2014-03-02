
open Str

exception Input_format_error of string;;

let parse channel =
	try

		(* Read the number & length declarations first *)

		let num_streams = int_of_string (input_line channel) in
		let stream_length = int_of_string (input_line channel) in	
		let streams = ref [] in
			try
			 	while true do 

			 		(* Get line, trim it, split it on space & convert to ints *)

			 		let trimmed = String.trim (input_line channel) in
			 		let split = Str.split (Str.regexp " ") trimmed in
			 		let stream = List.map int_of_string split in

			 			if List.length stream == stream_length then
			 				streams := stream :: !streams

							(* if stream list is now declared size raise end_of_file *)

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
