
open Lexing
open Parsing

exception Fatal of string;;
exception Undeclared_identifier of string;;
exception End_of_stream;;

exception Lexing_error of string;;
exception Parse_error of string;;

let get_source_line line = 
	let file = open_in Sys.argv.(1) in
		let rec internal current = 
			let sourceline = input_line file in 
				if line = current then
					sourceline
				else
					internal (current + 1)
		in 
			internal 1

let generate_error errortype message x y =
	let line = Str.global_replace (Str.regexp "\t") " " (get_source_line x.pos_lnum) in
	let left = x.pos_cnum - x.pos_bol in
	let right = y.pos_cnum - y.pos_bol in
	let location = Printf.sprintf " [Line %d:%d-%d] -> " x.pos_lnum left right in
	let length = max ((String.length location) + (String.length line) + 1) ((String.length message) + 2) in 
	let separator = (String.make length '=') ^ "\n" in

		"\n " ^ errortype ^ " Error\n" ^
		separator ^
		location ^ line ^ "\n" ^
		(String.make ((String.length location) + left) ' ') ^ "^" ^
		begin
			if right - left < 2 then
				""
			else
				(String.make (right - left - 2) '_') ^ "^"
		end
		^ "\n" ^
		separator ^
		" " ^ message ^ "\n"

let parse_err message =
	raise (Parse_error (
		generate_error 
			"Syntax"
			message 
			(rhs_start_pos 1) 
			(rhs_end_pos 1)))

let lexing_error token start finish = 
	raise (Lexing_error (generate_error "Lexing" token start finish))

