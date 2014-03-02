
open Language

(* Print out a stream list list (strings) *)

let rec getIntList = function
	| (i :: rest) -> string_of_int i ^ " " ^ getIntList rest
	| [] -> "";;

let printIntList intlist =
	print_endline (getIntList intlist);;

let rec printInput = function
	| (value :: rest) -> 
		printIntList value;
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
		print_endline (identifier ^ " = " ^ getIntList value ^ "; ");
		debugStreams rest
	| [] -> ();;

let rec debugBindings = function 
	| (identifier, value) :: rest -> 
		print_endline (identifier ^ " = " ^ (
			match value with 
				| Int (n) -> (string_of_int n)
				| _ -> "non integer value"
		));
		debugBindings rest
	| [] -> ();;