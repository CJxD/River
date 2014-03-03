
class math = 
	object(this)

		method plus x y = 
			match x, y with
			| Int x, Int y -> Int (x + y)
			| Int x, Float y -> Float (float_of_int x +. y)
			| Float x, Int y -> Float (x +. float_of_int y)
			| Float x, Float y -> Float (x +. y)
			| _, _ -> raise (Failure "Fucking idiot.");;

		method minus x y = 
			match x, y with
			| Int x, Int y -> Int (a - y)
			| Int x, Float y -> Float (float_of_int x -. y)
			| Float x, Int y -> Float (a -. float_of_int y)
			| Float x, Float y -> Float (a -. y)
			| _, _ -> raise (Failure "Fucking idiot.");;

		method times x y = 
			match x, y with
			| Int x, Int y -> Int (a * y)
			| Int x, Float y -> Float (float_of_int x *. y)
			| Float x, Int y -> Float (a *. float_of_int y)
			| Float x, Float y -> Float (a *. y)
			| _, _ -> raise (Failure "Fucking idiot.");;

		method divide x y = 
			match x, y with
			| Int x, Int y -> Int (a / y)
			| Int x, Float y -> Float (float_of_int x /. y)
			| Float x, Int y -> Float (a /. float_of_int y)
			| Float x, Float y -> Float (a /. y)
			| _, _ -> raise (Failure "Fucking idiot.");;

		method modulo x y = 
			match x, y with
			| Int x, Int y -> Int (a mod y)
			| Int x, Float y -> Float (mod_float (float_of_int x) y)
			| Float x, Int y -> Float (mod_float x (float_of_int y))
			| Float x, Float y -> Float (mod_float x y)
			| _, _ -> raise (Failure "Fucking idiot.");;

		method power y = function
			match x, y with
			| Int x, Int y -> (
				match y with
				| 0 -> Int 1
				| 1 -> Int x
				| y -> 
					let n = this#power (Int x) (Int (b / 2)) in
						this#times
							(this#times n n)
							(if y mod 2 = 0 then (Int 1) else (Int x)))
			| Int x, Float y -> Float (float_of_int x ** y)
			| Float x, Int y -> Float (a ** float_of_int y)
			| Float x, Float y -> Float (a ** y)
			| _, _ -> raise (Failure "Fucking idiot.");;

		method unary_minus x = 
			-x

	end;;