
open Lexing
open Parsing

exception Lexing_error of string;;
exception Parse_error of string;;

let generate_error message start finish =
	Printf.sprintf "[Line %d Character %d-%d] %s" 
		start.pos_lnum 
		(start.pos_cnum - start.pos_bol) 
		(finish.pos_cnum - finish.pos_bol) 
		message

let parse_err message nterm =
	raise (Parse_error (
		generate_error 
			message 
			(rhs_start_pos nterm) 
			(rhs_end_pos nterm)))

let lexing_error token start finish = 
	raise (Lexing_error (generate_error token start finish))

