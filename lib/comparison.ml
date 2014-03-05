
open Language

let equal x y = 
	match x, y with
	(* Numerics *)
	| Int x, Int y -> x = y
	| Int x, Float y -> (float_of_int x) = y
	| Float x, Int y -> x = (float_of_int y)
	| Float x, Float y -> x = y
	
	(* Characters *)
	| Char x, Char y -> x = y
	| Int x, Char y -> x = (Char.code y)
	| Char x, Int y -> (Char.code x) = y
	| Float x, Char y -> (int_of_float x) = (Char.code y)
	| Char x, Float y -> (Char.code x) = (int_of_float y)
	
	(* Booleans *)
	| Bool x, Bool y -> x = y
	| Bool x, Int y -> if x then y <> 0 else y = 0
	| Int x, Bool y -> if y then x <> 0 else x = 0
	| Bool x, Float y -> if x then y <> 0. else y = 0.
	| Float x, Bool y -> if y then x <> 0. else x = 0.
	| Bool x, Char y -> if x then (Char.code y) <> 0 else (Char.code y) = 0
	| Char x, Bool y -> if y then (Char.code x) <> 0 else (Char.code x) = 0

	(* Strings *)
	| String x, String y -> x = y
	| String x, Int y -> x = (string_of_int y)
	| Int x, String y -> (string_of_int x) = y
	| String x, Float y -> x = (string_of_float y)
	| Float x, String y -> (string_of_float x) = y
	| String x, Char y -> x = (String.make 1 y)
	| Char x, String y -> (String.make 1 x) = y
	| String x, Bool y -> if y then (String.length x) > 0 else (String.length x) = 0
	| Bool x, String y -> if x then (String.length y) > 0 else (String.length y) = 0

let not_equal x y = 
	match x, y with
	(* Numerics *)
	| Int x, Int y -> x <> y
	| Int x, Float y -> (float_of_int x) <> y
	| Float x, Int y -> x <> (float_of_int y)
	| Float x, Float y -> x <> y
	
	(* Characters *)
	| Char x, Char y -> x <> y
	| Int x, Char y -> x <> (Char.code y)
	| Char x, Int y -> (Char.code x) <> y
	| Float x, Char y -> (int_of_float x) <> (Char.code y)
	| Char x, Float y -> (Char.code x) <> (int_of_float y)
	
	(* Booleans *)
	| Bool x, Bool y -> x <> y
	| Bool x, Int y -> if x then y = 0 else y <> 0
	| Int x, Bool y -> if y then x = 0 else x <> 0
	| Bool x, Float y -> if x then y = 0. else y <> 0.
	| Float x, Bool y -> if y then x = 0. else x <> 0.
	| Bool x, Char y -> if x then (Char.code y) = 0 else (Char.code y) <> 0
	| Char x, Bool y -> if y then (Char.code x) = 0 else (Char.code x) <> 0

	(* Strings *)
	| String x, String y -> x <> y
	| String x, Int y -> x <> (string_of_int y)
	| Int x, String y -> (string_of_int x) <> y
	| String x, Float y -> x <> (string_of_float y)
	| Float x, String y -> (string_of_float x) <> y
	| String x, Char y -> x <> (String.make 1 y)
	| Char x, String y -> (String.make 1 x) <> y
	| String x, Bool y -> if y then (String.length x) = 0 else (String.length x) > 0
	| Bool x, String y -> if x then (String.length y) = 0 else (String.length y) > 0

let less_than x y = 
	match x, y with
	(* Numerics *)
	| Int x, Int y -> x < y
	| Int x, Float y -> (float_of_int x) < y
	| Float x, Int y -> x < (float_of_int y)
	| Float x, Float y -> x < y
	
	(* Characters *)
	| Char x, Char y -> x < y
	| Int x, Char y -> x < (Char.code y)
	| Char x, Int y -> (Char.code x) < y
	| Float x, Char y -> (int_of_float x) < (Char.code y)
	| Char x, Float y -> (Char.code x) < (int_of_float y)
	
	(* Booleans *)
	| Bool x, Bool y -> x = y
	| Bool x, Int y -> if x then y < 1 else y < 0
	| Int x, Bool y -> if y then x < 1 else x < 0
	| Bool x, Float y -> if x then y < 1. else y < 0.
	| Float x, Bool y -> if y then x < 1. else x < 0.
	| Bool x, Char y -> if x then (Char.code y) < 1 else (Char.code y) < 0
	| Char x, Bool y -> if y then (Char.code x) < 1 else (Char.code x) < 0

	| _, _ -> raise (Invalid_argument "You cannot use less than with string operands")

let greater_than x y = 
	match x, y with
	(* Numerics *)
	| Int x, Int y -> x > y
	| Int x, Float y -> (float_of_int x) > y
	| Float x, Int y -> x > (float_of_int y)
	| Float x, Float y -> x > y
	
	(* Characters *)
	| Char x, Char y -> x > y
	| Int x, Char y -> x > (Char.code y)
	| Char x, Int y -> (Char.code x) > y
	| Float x, Char y -> (int_of_float x) > (Char.code y)
	| Char x, Float y -> (Char.code x) > (int_of_float y)
	
	(* Booleans *)
	| Bool x, Bool y -> x = y
	| Bool x, Int y -> if x then y > 1 else y > 0
	| Int x, Bool y -> if y then x > 1 else x > 0
	| Bool x, Float y -> if x then y > 1. else y > 0.
	| Float x, Bool y -> if y then x > 1. else x > 0.
	| Bool x, Char y -> if x then (Char.code y) > 1 else (Char.code y) > 0
	| Char x, Bool y -> if y then (Char.code x) > 1 else (Char.code x) > 0

	| _, _ -> raise (Invalid_argument "You cannot use greater than with string operands")

let less_than_or_equal x y = 
	(less_than x y) || (equal x y)

let greater_than_or_equal x y = 
	(greater_than x y) || (equal x y)

let logical_and x y =
	match x, y with
		| Bool x, Bool y -> x && y
		| _, _ -> raise (Invalid_argument "You cannot use logical and on non boolean expressions")

let logical_or x y = 
	match x, y with
		| Bool x, Bool y -> x || y
		| _, _ -> raise (Invalid_argument "You cannot use logical or on non boolean expressions")