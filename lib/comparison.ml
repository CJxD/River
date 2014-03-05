
open Language

class comparison = 
	object(this)

		method equal x y = 
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

		method not_equal x y = 
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

		method less_than x y = 
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

		method greater_than x y = 
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

		method less_than_or_equal x y = 
			(this#less_than x y) || (this#equal x y)

		method greater_than_or_equal x y = 
			(this#greater_than x y) || (this#equal x y)

	end;;