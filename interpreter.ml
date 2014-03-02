
open Language
open Math
open Comparison
open Debug
	
class interpreter =
	object (this)

		val mutable streams = ([] : (string * int list) list)
		val mutable output = ([] : int list)

		method get_stream identifier = 
			try
				List.assoc identifier streams
			with
				Not_found -> 
					print_endline "ENTERED ERROR CONDITION:";
					Debug.debugAssocList streams;
					raise (Fatal ("Use of undeclared identifier: " ^ identifier))

		method get_default_stream = 
			if List.length streams == 1 then
				match List.hd streams with
					| (identifier, _) -> identifier
			else
				raise (Fatal "Omission of stream identifier in skip is forbidden when there is more than one stream defined")

		method string_of_int_list = function
			| value :: rest -> (string_of_int value) ^ " " ^ (this#string_of_int_list rest)
			| [] -> ""

		method get_output = 
			(string_of_int (List.length output)) ^ "\n" ^ 
			String.trim (this#string_of_int_list (List.rev output))

		method run program stream_list = 
			match program with 
				| Program (using, start, loop) -> 
					
					this#define_streams using stream_list;

					this#run_statement_list start;

					try
 
 						(* Main loop of the program, execute the loop body & advance the streams *)

						while true do 
							
							this#run_statement_list loop;

							List.iter this#skip_all streams;

						done
					with
						| End_of_stream -> ()

		method define_streams identifier_list stream_list =
			try
				match identifier_list with 
					| IdentifierList (identifier, rest) -> 
						streams <- (identifier, (List.nth stream_list (List.length streams))) :: streams;
						this#define_streams rest stream_list
					| EndIdentifier -> ()
			with
				| Failure e -> raise (Fatal "'with' decleration does not match the number of input streams")

		method run_statement_list = function
			| StatementList (statement, rest) -> 
				this#run_statement statement; 
				this#run_statement_list rest
			| EndStatement -> ()

		method skip number stream =
			if String.length stream == 0 then
				this#skip number this#get_default_stream
			else
				match number with
					| 0 -> ()
					| n -> 
						let skipped_stream = List.tl (this#get_stream stream) in
							streams <- List.remove_assoc stream streams;
							streams <- (stream, skipped_stream) :: streams; 	
							this#skip (number - 1) stream;

		method skip_all = function
			| (identifier, _) -> this#skip 1 identifier

		method output value =
			output <- value :: output

		method evaluate_expression expression = 
			match expression with
				| Literal (literal) -> 
					(match literal with 
						| Int n -> n
						| _ -> raise (Fatal "Invalid use of non-int literal in expression"))
				| Math (operation) ->
					this#run_math operation
				| Group (expression) -> 
					this#evaluate_expression expression
				| StreamAccess (stream, index) -> 
					try
						List.nth (this#get_stream stream) index 	
					with
						| Failure e -> raise End_of_stream
						

		method run_statement = function
			| Expression (expression) 	-> this#evaluate_expression expression; ()
			| Skip (elements, stream) 	-> this#skip elements stream
			| Output (expression) 		-> this#output (this#evaluate_expression expression)
			| If (condition, true_list, false_list) ->
				if this#evaluate_condition condition then
					this#run_statement_list true_list
				else
					this#run_statement_list false_list

		method evaluate_condition condition =
			let comparator = new comparison in
				match condition with
					| Equality (l, r)			-> comparator#int_equal 				(this#evaluate_expression l) (this#evaluate_expression r)
					| NonEquality (l, r) 		-> comparator#int_not_equal 			(this#evaluate_expression l) (this#evaluate_expression r)
					| LessThan (l, r) 			-> comparator#int_less_than 			(this#evaluate_expression l) (this#evaluate_expression r)
					| GreaterThan (l, r) 		-> comparator#int_greater_than 			(this#evaluate_expression l) (this#evaluate_expression r)
					| LessThanOrEqual (l, r) 	-> comparator#int_less_than_or_equal 	(this#evaluate_expression l) (this#evaluate_expression r)
					| GreaterThanOrEqual (l, r) -> comparator#int_greater_than_or_equal (this#evaluate_expression l) (this#evaluate_expression r)

		method run_math operation = 
			let math = new math in 
				match operation with 
					| Plus (l, r) 	-> math#plus 	(this#evaluate_expression l) (this#evaluate_expression r)
					| Minus (l, r) 	-> math#minus 	(this#evaluate_expression l) (this#evaluate_expression r)
					| Times (l, r) 	-> math#times 	(this#evaluate_expression l) (this#evaluate_expression r)
					| Divide (l, r) -> math#divide 	(this#evaluate_expression l) (this#evaluate_expression r)
					| Modulo (l, r) -> math#modulo 	(this#evaluate_expression l) (this#evaluate_expression r)
					| Power (l, r) 	-> math#power 	(this#evaluate_expression l) (this#evaluate_expression r)

	end;;