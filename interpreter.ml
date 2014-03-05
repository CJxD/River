
open Language
open Math
open Comparison
open Debug
open Input

class interpreter =
	object (this)

		val mutable streams = ([] : (string * literal list) list)
		val mutable output = ([] : literal list)
		val mutable bindings = ([] : (string * literal) list)

		method get_stream identifier = 
			try
				List.assoc identifier streams
			with
				Not_found -> 
					raise (Undeclared_identifier identifier)

		method get_default_stream = 
			if List.length streams == 1 then
				match List.hd streams with
					| (identifier, _) -> identifier
			else
				raise (Fatal "Omission of stream identifier in skip is forbidden when there is more than one stream defined")

		method string_of_list = function
			| Int value :: rest -> (string_of_int value) ^ " " ^ (this#string_of_list rest)
			| Float value :: rest -> (string_of_float value) ^ " " ^ (this#string_of_list rest)
			| Char value :: rest -> (String.make 1 value) ^ " " ^ (this#string_of_list rest)
			| Bool value :: rest -> (string_of_bool value) ^ " " ^ (this#string_of_list rest)
			| [] -> ""

		method get_output = 
			(string_of_int (List.length output)) ^ "\n" ^ string_trim (this#string_of_list output)

		method run program stream_list = 
			match program with 
				| Program (using, start, loop) -> 
					
					this#define_streams using stream_list;

					this#run_statement_list start;

					try
 
 						(* Main loop of the program, execute the loop body & advance the streams *)

						while true do 

							this#run_statement_list loop;

							List.iter this#skip_stream_by_one streams;

						done
					with
						| End_of_stream -> ()

		method define_streams identifier_list stream_list =
			try
				match identifier_list with 
					| identifier :: rest -> 
						streams <- (identifier, (List.nth stream_list (List.length streams))) :: streams;
						this#define_streams rest stream_list
					| [] -> ()
			with
				| Failure e -> raise (Fatal "'with' decleration does not match the number of input streams")

		method run_statement_list = function
			| statement :: rest -> 
				this#run_statement statement; 
				this#run_statement_list rest
			| [] -> ()

		method skip number stream =
			if String.length stream == 0 then
				this#skip number this#get_default_stream
			else
				match number with
					| 0 -> ()
					| n -> 
						try 
							let skipped_stream = List.tl (this#get_stream stream) in
								streams <- List.remove_assoc stream streams;
								streams <- (stream, skipped_stream) :: streams; 	
								this#skip (number - 1) stream;
						with
							Failure e -> raise End_of_stream

		method skip_stream_by_one = function
			| (identifier, _) -> 
				this#skip 1 identifier;
				if List.length (this#get_stream identifier) == 0 then
					raise End_of_stream

		method output value =
			output <- output @ [value]

		method update_binding identifier value = 
			bindings <- (identifier, value) :: List.remove_assoc identifier bindings;
			value

		method read_binding identifier =
			try
				List.assoc identifier bindings
			with
				Not_found -> 
					raise (Undeclared_identifier identifier)

		method evaluate_expression expression = 
			match expression with
				| Literal (literal) ->
					literal
				| Identifier (identifier) ->
					this#read_binding identifier
				| Application (identifier, arguments) ->
					this#apply_function identifier arguments
				| BinaryOperation (operation, left, right) ->
					this#run_binary_operation operation left right
				| UnaryOperation (operation, expression) ->
					this#run_unary_operation operation expression
				| VariableOperation (operation, identifier) ->
					this#run_variable_operation operation identifier
				| Assignment (optype, identifier, value) ->
					this#run_assignment optype identifier value
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
				match condition with 
					| Condition (test, left, right) ->
						if this#evaluate_condition test left right then
							this#run_statement_list true_list
						else
							this#run_statement_list false_list

		method evaluate_condition test left right =
			let comparator = new comparison in
			let x = this#evaluate_expression left in
			let y = this#evaluate_expression right in 
				match test with
					| Equality 				-> comparator#equal x y 
					| NonEquality 			-> comparator#not_equal x y 
					| LessThan 				-> comparator#less_than x y 
					| GreaterThan 			-> comparator#greater_than x y 
					| LessThanOrEqual 		-> comparator#less_than_or_equal x y
					| GreaterThanOrEqual 	-> comparator#greater_than_or_equal x y

		method run_binary_operation operation left right = 
			let math = new math in
			let x = this#evaluate_expression left in
			let y = this#evaluate_expression right in
				match operation with 
					| Plus 		-> math#plus x y
					| Minus 	-> math#minus x y
					| Times 	-> math#times x y
					| Divide 	-> math#divide x y
					| Modulo 	-> math#modulo x y
					| Power 	-> math#power x y 

		method run_unary_operation operation expression = 
			let math = new math in
			let x = this#evaluate_expression expression in 
				match operation with 
					| UnaryMinus -> math#unary_minus x

		method run_assignment optype identifier expression =
			let evaluated = this#evaluate_expression expression in
			match optype with 
				| StandardAssign -> 
					this#update_binding identifier evaluated;
					evaluated

				(* StandardAssign & operation assigns need to be seperated,
				   we want to check if the variable is assigned for operation assigns, 
				   but we need to allow new variables for standard assigns *)

				| _ ->
					let math = new math in
					let n = this#read_binding identifier in 
						match optype with
							| PlusAssign ->
								this#update_binding identifier (math#plus n evaluated)
							| MinusAssign ->
								this#update_binding identifier (math#minus n evaluated)
							| TimesAssign ->
								this#update_binding identifier (math#times n evaluated)
							| DivideAssign ->
								this#update_binding identifier (math#divide n evaluated)

		method run_variable_operation operation identifier =
			let math = new math in
			let n = this#read_binding identifier in
			let one = Literal (Int 1) in
				match operation with 
					| PostfixIncrement -> 
						this#run_assignment PlusAssign identifier one; 
						n
					| PostfixDecrement -> 
						this#run_assignment MinusAssign identifier one;
						n
					| PrefixIncrement ->
						this#run_assignment PlusAssign identifier one;
						math#plus n (Int 1)
					| PrefixDecrement ->
						this#run_assignment MinusAssign identifier one;
						math#minus n (Int 1)
		
		method apply_function identifier argument_list =
			let arguments = List.map this#evaluate_expression argument_list in
			let comparison = new comparison in 
				try
					match identifier with
						| "min" -> 
							if comparison#less_than (List.nth arguments 0) (List.nth arguments 1) then
								(List.nth arguments 0)
							else 
								(List.nth arguments 1)
						| "max" ->
							if comparison#greater_than (List.nth arguments 0) (List.nth arguments 1) then
								(List.nth arguments 0)
							else 
								(List.nth arguments 1)
				with
					| Failure e -> 
						match e with 
							| "nth" -> raise (Fatal ("Incorrect number of arguments specified for " ^ identifier))

	end;;