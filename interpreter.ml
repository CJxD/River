
exception Fatal of string;;
exception End_of_stream;;
exception Undeclared_identifier of string;;

open Language
open Math
open Comparison
open Input

class interpreter =
	object (this)

		val mutable streams = ([] : (string * literal list) list)
		val mutable bindings = ([] : (string * literal) list)
		val mutable output = ([] : literal list)

		(* Stream Bindings *)

		method get_stream identifier = 
			try
				List.assoc identifier streams
			with
				Not_found -> 
					if List.mem_assoc identifier bindings then
						raise (Fatal ("Use of variable " ^ identifier ^ " in stream context is not allowed"))
					else
						raise (Undeclared_identifier identifier)

		method get_default_stream_identifier = 
			if List.length streams == 1 then
				match List.hd streams with
					| (identifier, _) -> identifier
			else
				raise (Fatal "Omission of stream identifier in skip is forbidden when there is more than one stream defined")

		method define_streams identifier_list stream_list =
			try
				match identifier_list with 
					| identifier :: rest -> 
						streams <- (identifier, (List.nth stream_list (List.length streams))) :: streams;
						this#define_streams rest stream_list
					| [] -> ()
			with
				| Failure e -> raise (Fatal "'with' decleration does not match the number of input streams")

		(* Output *)

		method string_of_list = function
			| Int value :: rest -> (string_of_int value) ^ " " ^ (this#string_of_list rest)
			| Float value :: rest -> (string_of_float value) ^ " " ^ (this#string_of_list rest)
			| Char value :: rest -> (String.make 1 value) ^ " " ^ (this#string_of_list rest)
			| Bool value :: rest -> (string_of_bool value) ^ " " ^ (this#string_of_list rest)
			| String value :: rest -> "'" ^ value ^ "' " ^ (this#string_of_list rest)
			| [] -> ""

		method get_output = 
			(string_of_int (List.length output)) ^ "\n" ^ string_trim (this#string_of_list output)

		method output = function 
			| String s -> raise (Invalid_argument "You cannot pass a string literal to out")
			| value -> output <- output @ [value]

		method print_literal = function 
			| Int n -> print_string (string_of_int n)
			| Float n -> print_string (string_of_float n)
			| Bool b -> print_string (string_of_bool b)
			| Char c -> print_string (String.make 1 c)
			| String s -> print_string s

		(* Stream Skipping operations *)

		method skip number stream =
			if String.length stream == 0 then
				this#skip number this#get_default_stream_identifier
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

		(* Bindings read/write *)

		method read_binding identifier =
			try
				List.assoc identifier bindings
			with
				Not_found ->
					if List.mem_assoc identifier streams then
						raise (Fatal ("Use of stream " ^ identifier ^ " in variable context is not allowed. Did you forget ~?"))
					else
						raise (Undeclared_identifier identifier)

		method update_binding identifier value = 
			bindings <- (identifier, value) :: List.remove_assoc identifier bindings;
			value

		(* Interpreter *)
					
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

		method run_statement_list = function
			| statement :: rest -> 
				this#run_statement statement; 
				this#run_statement_list rest
			| [] -> ()

		method run_statement = function
			| Expression (expression) 	-> this#evaluate_expression expression; ()
			| Skip (elements, stream) 	-> this#skip elements stream
			| Output (expression) 		-> this#output (this#evaluate_expression expression)
			| If (condition, true_list, false_list) ->
				if this#evaluate_condition condition then
					this#run_statement_list true_list
				else
					this#run_statement_list false_list

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

		method evaluate_condition condition =
			match condition with 
				| UnaryCondition test -> 
					begin
						match test with 
							| Test (optype, left, right) -> this#evaluate_test optype left right
					end
				| BinaryCondition (condtype, left, right) ->
					let x = this#evaluate_condition left in
					let y = this#evaluate_condition right in
						match condtype with 
							| LogicalAnd -> x && y
							| LogicalOr -> x || y

		method evaluate_test test left right =
			let x = this#evaluate_expression left in
			let y = this#evaluate_expression right in 
				match test with
					| Equality 				-> Comparison.equal x y 
					| NonEquality 			-> Comparison.not_equal x y 
					| LessThan 				-> Comparison.less_than x y 
					| GreaterThan 			-> Comparison.greater_than x y 
					| LessThanOrEqual 		-> Comparison.less_than_or_equal x y
					| GreaterThanOrEqual 	-> Comparison.greater_than_or_equal x y

		method run_binary_operation operation left right = 
			let x = this#evaluate_expression left in
			let y = this#evaluate_expression right in
				match operation with 
					| Plus 		-> Math.plus x y
					| Minus 	-> Math.minus x y
					| Times 	-> Math.times x y
					| Divide 	-> Math.divide x y
					| Modulo 	-> Math.modulo x y
					| Power 	-> Math.power x y 

		method run_unary_operation operation expression = 
			let x = this#evaluate_expression expression in 
				match operation with 
					| UnaryMinus -> Math.unary_minus x

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
					let n = this#read_binding identifier in 
						match optype with
							| PlusAssign ->
								this#update_binding identifier (Math.plus n evaluated)
							| MinusAssign ->
								this#update_binding identifier (Math.minus n evaluated)
							| TimesAssign ->
								this#update_binding identifier (Math.times n evaluated)
							| DivideAssign ->
								this#update_binding identifier (Math.divide n evaluated)

		method run_variable_operation operation identifier =
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
						Math.plus n (Int 1)
					| PrefixDecrement ->
						this#run_assignment MinusAssign identifier one;
						Math.minus n (Int 1)
		
		method apply_function identifier argument_list =
			let arguments = List.map this#evaluate_expression argument_list in
				try
					match identifier with
						| "min" 	-> Math.min (List.nth arguments 0) (List.nth arguments 1)
						| "max" 	-> Math.max (List.nth arguments 0) (List.nth arguments 1)
						| "average" -> Math.average arguments
						| "round" 	-> Math.round (List.nth arguments 0)
						| "floor" 	-> Math.floor (List.nth arguments 0)
						| "ceil" 	-> Math.ceil (List.nth arguments 0)
						| "debug" -> 
							List.map (fun l -> this#print_literal l; print_string " ") arguments; 
							print_endline ""; 
							Int 0
						| _ -> raise (Fatal ("Call to undeclared function " ^ identifier))
				with
					| Failure e -> 
						match e with 
							| "nth" -> raise (Fatal ("Incorrect number of arguments specified for " ^ identifier))
							| _ -> raise (Fatal ("Function exception: " ^ e))

	end;;