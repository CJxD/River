
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

let contains interpreter variable stream element =
	match stream with 
		| Stream (stream) -> Bool (List.mem element stream)
		| _ -> raise (Fatal ("You cannot use .contains() on non stream variable " ^ variable ^ "."))

let append interpreter variable stream element =
	match stream with 
		| Stream (stream) -> 
			interpreter#update_binding variable (Stream (stream @ [ element ]));
			Bool true
		| _ -> raise (Fatal ("You cannot append elements to non stream variable " ^ variable ^ "."))

let remove interpreter variable stream element = 
	match stream with 
		| Stream (stream) -> 
			interpreter#update_binding variable (Stream (List.filter (fun l -> l <> element) stream));
			Bool true
		| _ -> raise (Fatal ("You cannot remove elements from non stream variable " ^ variable ^ "."))

let length interpreter variable stream =
	match stream with 
		| Stream (stream) -> 
			Int (List.length stream)
		| _ -> raise (Fatal ("Non stream variable " ^ variable ^ " does not have a length."))

let get interpreter variable stream index =
	match stream with 
		| Stream (stream) ->
			begin
				match index with 
					| Int (n) ->
						begin 
							try
								List.nth stream n
							with 
								| Failure e -> 
									raise (Fatal (
										"Stream index out of bounds (" ^ (string_of_int n) ^ 
										" in " ^ variable))
						end
					| _ -> raise (Fatal ("Call to " ^ variable ^ ".get() was passed a non numeric index."))
			end
		| _ -> raise (Fatal ("Non stream variable " ^ variable ^ " cannot be accessed by index."))

let head interpreter variable stream = 
	match stream with 
		| Stream (stream) -> List.hd stream
		| _ -> raise (Fatal ("Non stream variable " ^ variable ^ " has no head."))

let tail interpreter variable stream =
	match stream with 
		| Stream (stream) -> List.hd stream
		| _ -> raise (Fatal ("Non stream variable " ^ variable ^ " has no tail."))