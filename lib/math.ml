
open Language
open Comparison

let plus x y = 
	match x, y with
	| Int x, Int y -> Int (x + y)
	| Int x, Float y -> Float (float_of_int x +. y)
	| Float x, Int y -> Float (x +. float_of_int y)
	| Float x, Float y -> Float (x +. y)
	| _, _ -> raise (Invalid_argument "you may only perform math operations on numeric types.")

let minus x y = 
	match x, y with
	| Int x, Int y -> Int (x - y)
	| Int x, Float y -> Float (float_of_int x -. y)
	| Float x, Int y -> Float (x -. float_of_int y)
	| Float x, Float y -> Float (x -. y)
	| _, _ -> raise (Invalid_argument "you may only perform math operations on numeric types.")

let times x y = 
	match x, y with
	| Int x, Int y -> Int (x * y)
	| Int x, Float y -> Float (float_of_int x *. y)
	| Float x, Int y -> Float (x *. float_of_int y)
	| Float x, Float y -> Float (x *. y)
	| _, _ -> raise (Invalid_argument "you may only perform math operations on numeric types.")

let divide x y = 
	match x, y with
	| Int x, Int y -> Int (x / y)
	| Int x, Float y -> Float (float_of_int x /. y)
	| Float x, Int y -> Float (x /. float_of_int y)
	| Float x, Float y -> Float (x /. y)
	| _, _ -> raise (Invalid_argument "you may only perform math operations on numeric types.")

let modulo x y = 
	match x, y with
	| Int x, Int y -> Int (x mod y)
	| Int x, Float y -> Float (mod_float (float_of_int x) y)
	| Float x, Int y -> Float (mod_float x (float_of_int y))
	| Float x, Float y -> Float (mod_float x y)
	| _, _ -> raise (Invalid_argument "you may only perform math operations on numeric types.")

let rec power x y =
	match x, y with
	| Int x, Int y -> (
		match y with
		| 0 -> Int 1
		| 1 -> Int x
		| y -> 
			let n = power (Int x) (Int (y / 2)) in
				times
					(times n n)
					(if y mod 2 = 0 then (Int 1) else (Int x)))
	| Int x, Float y -> Float (float_of_int x ** y)
	| Float x, Int y -> Float (x ** float_of_int y)
	| Float x, Float y -> Float (x ** y)
	| _, _ -> raise (Invalid_argument "you may only perform math operations on numeric types.")

let min x y =
	if (Comparison.less_than x y) then x else y
	
let max x y =
	if (Comparison.greater_than x y) then x else y
	
let unary_minus x =
	match x with
	| Int x -> Int (0 - x)
	| Float x -> Float (0. -. x)
	| _ -> raise (Invalid_argument "you may only perform math operations on numeric types.")

let rec average values =
	divide (List.fold_left plus (Float 0.0) values) (Int (List.length values))

let round = function
	| Float n -> Int (int_of_float (floor (n +. 0.5)))
	| Int n -> Int n
	| _ -> raise (Invalid_argument "You can only round numeric types")

let floor = function
	| Float n -> Float (floor n)
	| Int n -> Float (float_of_int n)
	| _ -> raise (Invalid_argument "You can only floor numeric types")

let ceil = function
	| Float n -> Float (ceil n)
	| Int n -> Float (float_of_int n)
	| _ -> raise (Invalid_argument "You can only ceil numeric types")