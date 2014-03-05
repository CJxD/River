
open Language

(* Print out a stream list list (strings) *)

let rec getList = function
	| Int value :: rest -> (string_of_int value) ^ " " ^ (getList rest)
	| Float value :: rest -> (string_of_float value) ^ " " ^ (getList rest)
	| Char value :: rest -> (String.make 1 value) ^ " " ^ (getList rest)
	| Bool value :: rest -> (string_of_bool value) ^ " " ^ (getList rest)
	| [] -> "";;

let printList l =
	print_endline (getList l);;

let rec printInput = function
	| (value :: rest) -> 
		printList value;
		printInput rest
	| [] -> ();; 

let getLiteral = function
	  Int (n) 		-> "int(" ^ string_of_int n ^ ")"
	| Float (n) 	-> "float(" ^ string_of_float n ^ ")"
	| Bool (true) 	-> "bool(true)"
	| Bool (false) 	-> "bool(false)"
	| Char (n) 		-> "char(" ^ Char.escaped n ^ ")";;

let rec debugStreams = function
	| (identifier, value) :: rest -> 
		print_endline (identifier ^ " = " ^ getList value ^ "; ");
		debugStreams rest
	| [] -> ();;

let rec debugBindings = function 
	| (identifier, value) :: rest -> 
		print_endline (identifier ^ " = " ^ (
			match value with 
				| Int (n) -> (string_of_int n)
				| Float (n) -> (string_of_float n)
				| Char (n) -> (String.make 1 n)
				| Bool (n) -> (string_of_bool n)
		));
		debugBindings rest
	| [] -> ();;