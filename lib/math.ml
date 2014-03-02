
class math = 
	object(this)

		method plus x y = 
			x + y

		method minus x y = 
			x - y

		method times x y = 
			x * y

		method divide x y = 
			x / y

		method modulo x y = 
			x mod y

		method power y = function
			| 0 -> 1
			| 1 -> y
			| n -> 
				let b = this#power y (n / 2) in
					b * b * (if n mod 2 = 0 then 1 else y)

	end;;