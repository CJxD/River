
open Language
open Errors
open Math
open Comparison
open Input
open Streams

class interpreter =
	object (this)

		val mutable bindings = ([] : (string * literal) list)
		val mutable inputs = ([] : string list)
		val output = ("_out" : string)

		(* Bindings read/write *)

		method read_binding identifier =
			try
				List.assoc identifier bindings
			with
				Not_found ->
					raise (Undeclared_identifier identifier)

		method update_binding identifier value = 
			bindings <- (identifier, value) :: List.remove_assoc identifier bindings;
			value

		(* Stream Bindings *)

		method get_default_stream_identifier = 
			if List.length inputs == 1 then
				List.hd inputs
			else
				raise (Fatal "Omission of stream identifier in skip is forbidden when there is more than one stream defined")

		method define_streams identifier_list stream_list =
			try
				match identifier_list with 
					| identifier :: rest -> 
						bindings <- (identifier, (List.nth stream_list (List.length bindings))) :: bindings;
						inputs <- identifier :: inputs;
						this#define_streams rest stream_list
					| [] -> ()
			with
				| Failure e -> raise (Fatal "'with' decleration does not match the number of input streams")

		(* Output *)

		method define_output =
			this#update_binding output (Stream [])

		method get_output = 
			match this#read_binding output with
			| Stream stream ->
				(string_of_int (List.length stream)) ^ "\n" ^ string_trim (Streams.string_of_stream stream)
			| literal ->
				Streams.string_of_literal literal

		(* Stream continuation operations *)

		method next_all =
			this#next inputs

		method next = function
			| identifier :: rest ->
				begin
					match this#read_binding identifier with
					| Stream s ->
						this#update_binding identifier (Streams.skip 1 s);
						()
					| _ -> ()
				end;
				this#next rest
			| [] -> ()

		(* Interpreter *)
					
		method run program stream_list = 
			match program with 
				| Program (using, start, loop) -> 
					
					this#define_streams using stream_list;
					this#define_output;

					this#run_statement_list start;

					try
 
 						(* Main loop of the program, execute the loop body & advance the streams *)

						while true do 

							this#run_statement_list loop;

							this#next_all;

						done
					with
						| End_of_stream -> ()

		method run_statement_list = function
			| statement :: rest -> 
				this#run_statement statement; 
				this#run_statement_list rest
			| [] -> ()

		method run_statement = function
			| Expression (expression) ->
				this#evaluate_expression expression; ()

			| Output (expression) -> 
				begin
					match this#read_binding output with
					| Stream s ->
						this#update_binding output
							(Streams.out
								s
								(this#evaluate_expression expression))
					| _ -> raise (Fatal "output buffer is not a stream!")
				end;
				()

			| Skip (n, stream) ->
				let name =
					if String.length stream = 0 then
						this#get_default_stream_identifier
					else
						stream
				in begin
					match this#read_binding name with
					| Stream s -> this#update_binding name (Streams.skip n s); ()
					| _ -> raise (Fatal "cannot skip non-streams")
				end

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
				| StreamConstruction (expressions) ->
					this#construct_stream expressions
				| StreamAccess (stream, index) -> 
					try
						match (this#read_binding stream) with
						| Stream s ->
							List.nth s index 	
						| String s ->
							Char (String.get s index)
						| _ -> raise (Fatal "indexing is only allowed for streams and strings")
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
							List.map (fun l -> print_string ((Streams.string_of_literal l) ^ " ")) arguments;
							print_endline ""; 
							Int 0
						| _ -> raise (Fatal ("Call to undeclared function " ^ identifier))
				with
					| Failure e -> 
						match e with 
							| "nth" -> raise (Fatal ("Incorrect number of arguments specified for " ^ identifier))
							| _ -> raise (Fatal ("Function exception: " ^ e))

		method construct_stream expressions = 
			Stream (
				let rec internal = function 
					| expression :: rest -> (this#evaluate_expression expression) :: (internal rest)
					| [] -> []
				in 
					internal expressions
			)
	end;;
