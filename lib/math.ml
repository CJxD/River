
open Language
open Comparison

exception Illegal_argument of string;;

class math = 
	object(this)

		method plus x y = 
			match x, y with
			| Int x, Int y -> Int (x + y)
			| Int x, Float y -> Float (float_of_int x +. y)
			| Float x, Int y -> Float (x +. float_of_int y)
			| Float x, Float y -> Float (x +. y)
			| _, _ -> raise (Illegal_argument "you may only perform math operations on numeric types.")

		method minus x y = 
			match x, y with
			| Int x, Int y -> Int (x - y)
			| Int x, Float y -> Float (float_of_int x -. y)
			| Float x, Int y -> Float (x -. float_of_int y)
			| Float x, Float y -> Float (x -. y)
			| _, _ -> raise (Illegal_argument "you may only perform math operations on numeric types.")

		method times x y = 
			match x, y with
			| Int x, Int y -> Int (x * y)
			| Int x, Float y -> Float (float_of_int x *. y)
			| Float x, Int y -> Float (x *. float_of_int y)
			| Float x, Float y -> Float (x *. y)
			| _, _ -> raise (Illegal_argument "you may only perform math operations on numeric types.")

		method divide x y = 
			match x, y with
			| Int x, Int y -> Int (x / y)
			| Int x, Float y -> Float (float_of_int x /. y)
			| Float x, Int y -> Float (x /. float_of_int y)
			| Float x, Float y -> Float (x /. y)
			| _, _ -> raise (Illegal_argument "you may only perform math operations on numeric types.")

		method modulo x y = 
			match x, y with
			| Int x, Int y -> Int (x mod y)
			| Int x, Float y -> Float (mod_float (float_of_int x) y)
			| Float x, Int y -> Float (mod_float x (float_of_int y))
			| Float x, Float y -> Float (mod_float x y)
			| _, _ -> raise (Illegal_argument "you may only perform math operations on numeric types.")

		method power x y =
			match x, y with
			| Int x, Int y -> (
				match y with
				| 0 -> Int 1
				| 1 -> Int x
				| y -> 
					let n = this#power (Int x) (Int (y / 2)) in
						this#times
							(this#times n n)
							(if y mod 2 = 0 then (Int 1) else (Int x)))
			| Int x, Float y -> Float (float_of_int x ** y)
			| Float x, Int y -> Float (x ** float_of_int y)
			| Float x, Float y -> Float (x ** y)
			| _, _ -> raise (Illegal_argument "you may only perform math operations on numeric types.")

		method min x y =
			let comp = new comparison in
			if (comp#less_than x y) then x else y
			
		method max x y =
			let comp = new comparison in
			if (comp#greater_than x y) then x else y
			
		method unary_minus x =
			match x with
			| Int x -> Int (0 - x)
			| Float x -> Float (0. -. x)
			| _ -> raise (Illegal_argument "you may only perform math operations on numeric types.")
			
	end;;