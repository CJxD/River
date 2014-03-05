
open Language
open Comparison

exception Illegal_argument of string;;

let plus x y = 
	match x, y with
	| Int x, Int y -> Int (x + y)
	| Int x, Float y -> Float (float_of_int x +. y)
	| Float x, Int y -> Float (x +. float_of_int y)
	| Float x, Float y -> Float (x +. y)
	| _, _ -> raise (Illegal_argument "you may only perform math operations on numeric types.")

let minus x y = 
	match x, y with
	| Int x, Int y -> Int (x - y)
	| Int x, Float y -> Float (float_of_int x -. y)
	| Float x, Int y -> Float (x -. float_of_int y)
	| Float x, Float y -> Float (x -. y)
	| _, _ -> raise (Illegal_argument "you may only perform math operations on numeric types.")

let times x y = 
	match x, y with
	| Int x, Int y -> Int (x * y)
	| Int x, Float y -> Float (float_of_int x *. y)
	| Float x, Int y -> Float (x *. float_of_int y)
	| Float x, Float y -> Float (x *. y)
	| _, _ -> raise (Illegal_argument "you may only perform math operations on numeric types.")

let divide x y = 
	match x, y with
	| Int x, Int y -> Int (x / y)
	| Int x, Float y -> Float (float_of_int x /. y)
	| Float x, Int y -> Float (x /. float_of_int y)
	| Float x, Float y -> Float (x /. y)
	| _, _ -> raise (Illegal_argument "you may only perform math operations on numeric types.")

let modulo x y = 
	match x, y with
	| Int x, Int y -> Int (x mod y)
	| Int x, Float y -> Float (mod_float (float_of_int x) y)
	| Float x, Int y -> Float (mod_float x (float_of_int y))
	| Float x, Float y -> Float (mod_float x y)
	| _, _ -> raise (Illegal_argument "you may only perform math operations on numeric types.")

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
	| _, _ -> raise (Illegal_argument "you may only perform math operations on numeric types.")

let min x y =
	if (Comparison.less_than x y) then x else y
	
let max x y =
	if (Comparison.greater_than x y) then x else y
	
let unary_minus x =
	match x with
	| Int x -> Int (0 - x)
	| Float x -> Float (0. -. x)
	| _ -> raise (Illegal_argument "you may only perform math operations on numeric types.")
