
open Language

(*
 * Append an element to the tail of a stream
 * acc - Accumulator stream
 * element - Element to add to accumulator
 *)
let out acc element = 
	acc @ [element];;
	
(*
 * Prepend an element to the head of a stream
 * acc - Accumulator stream
 * element - Element to add to accumulator
 *)
let prepend acc element = 
	element :: acc;;
	
(*
 * Truncate elements from the head of a stream
 * stream - Stream to truncate
 * n - Number of elements to remove
 *)
let rec skip stream n = 
	if n <= 0 then
		stream
	else
		if n <= 1 then
			List.tl stream
		else
			skip (List.tl stream) (n-1);;
			
let add a b =
	match a, b with
	| Int a, Int b -> Int (a + b)
	| Int a, Float b -> Float (float_of_int a +. b)
	| Float a, Int b -> Float (a +. float_of_int b)
	| Float a, Float b -> Float (a +. b)
	| _, _ -> raise (Failure "Fucking idiot.");;
	
let sub a b =
	match a, b with
	| Int a, Int b -> Int (a - b)
	| Int a, Float b -> Float (float_of_int a -. b)
	| Float a, Int b -> Float (a -. float_of_int b)
	| Float a, Float b -> Float (a -. b)
	| _, _ -> raise (Failure "Fucking idiot.");;
	
let mul a b =
	match a, b with
	| Int a, Int b -> Int (a * b)
	| Int a, Float b -> Float (float_of_int a *. b)
	| Float a, Int b -> Float (a *. float_of_int b)
	| Float a, Float b -> Float (a *. b)
	| _, _ -> raise (Failure "Fucking idiot.");;
	
let div a b =
	match a, b with
	| Int a, Int b -> Int (a / b)
	| Int a, Float b -> Float (float_of_int a /. b)
	| Float a, Int b -> Float (a /. float_of_int b)
	| Float a, Float b -> Float (a /. b)
	| _, _ -> raise (Failure "Fucking idiot.");;
	
let modulo a b =
	match a, b with
	| Int a, Int b -> Int (a mod b)
	| Int a, Float b -> Float (mod_float (float_of_int a) b)
	| Float a, Int b -> Float (mod_float a (float_of_int b))
	| Float a, Float b -> Float (mod_float a b)
	| _, _ -> raise (Failure "Fucking idiot.");;
	
let rec pow a b =
	match a, b with
	| Int a, Int b -> (
		match b with
		| 0 -> Int 1
		| 1 -> Int a
		| n -> 
			let b = pow (Int a) (Int (n / 2)) in
				mul (mul b b) (if n mod 2 = 0 then (Int 1) else (Int a)))
	| Int a, Float b -> Float (float_of_int a ** b)
	| Float a, Int b -> Float (a ** float_of_int b)
	| Float a, Float b -> Float (a ** b)
	| _, _ -> raise (Failure "Fucking idiot.");;

let assign identifier literal =
	print_endline ("assigning " ^ (getLiteral literal) ^ " to variable " ^ identifier);;

let apply identifier expression =
	print_endline ("applying expression " ^ (getExpression expression) ^ " as argument to function " ^ identifier);;

let run_expression = function  
	| Literal (_) -> ()
	| VariableRead (_) -> ()
	| Assignment (Variable (identifier), literal) -> assign identifier literal
	| Application (Function (identifier), expression) -> apply identifier expression
	| _ -> ();;

let run_statement = function
	| Expression (expression) -> run_expression expression
	| If (_, _, _) -> ()
	| While (_, _) -> ();;

let rec run = function
	| Nothing 								-> ()
	| StatementList (statement, Nothing) 	-> run_statement statement
	| StatementList (statement, rest) 		-> run_statement statement; run rest;;
