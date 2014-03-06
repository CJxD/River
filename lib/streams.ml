
open Language
open Errors

let out stream value =
	Stream (
		match value with
		| Stream s -> stream @ s
		| value -> stream @ [value]
	)

let rec skip number stream =
	match number with
		| 0 -> Stream stream
		| n -> 
			try 
				skip (n - 1) (List.tl stream)
			with
				Failure e -> raise End_of_stream

let rec string_of_literal = function
	| Int n -> string_of_int n
	| Float n -> string_of_float n
	| Bool b -> string_of_bool b
	| Char c -> String.make 1 c
	| String s -> s
	| Stream s -> string_of_stream s

and string_of_stream = function
	(* string_of_stream adds quotes, where as string_of_literal does not *)
	| Int value :: rest -> string_of_literal (Int value) ^ " " ^ string_of_stream rest
	| Float value :: rest -> string_of_literal (Float value) ^ " " ^ string_of_stream rest
	| Char value :: rest -> "'" ^ string_of_literal (Char value) ^ "' " ^ string_of_stream rest
	| Bool value :: rest -> string_of_literal (Bool value) ^ " " ^ string_of_stream rest
	| String value :: rest -> "\"" ^ string_of_literal (String value) ^ "\" " ^ string_of_stream rest
	| Stream values :: rest -> "[" ^ string_of_literal (Stream values) ^ "]" ^ string_of_stream rest
	| [] -> ""