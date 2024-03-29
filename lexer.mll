{
	open Parser
	open Lexing
	open Errors
}

let white_space = [' ' '\t']
let digit = ['0'-'9']
let int = digit +
let float = (int '.' int) | (int ['f' 'F'])
let char = ''' [^ '\n'] '''
let bool = "true" | "false"
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
	
	(* Lines & Whitespace *)

	  white_space 			{ token lexbuf }
	| '\n' 					{ new_line lexbuf; token lexbuf }
	| eof 					{ EOF }
	| ';' 					{ EOL }

	(* Comments *)

	| "//" [^ '\n']* 		{ token lexbuf }
	
	(* | "/*" _* "*/" 			{ token lexbuf } *)

	(* Literals *)

	| int 					{ INT(int_of_string (lexeme lexbuf)) }
	| float 				{ FLOAT(float_of_string (lexeme lexbuf)) }
	| char 					{ CHAR(String.get (lexeme lexbuf) 1) }
	| bool 					{ BOOL(bool_of_string (lexeme lexbuf)) }
	| "true" 				{ TRUE }
	| "false" 				{ FALSE }
	| '"' [^ '"']* '"'		{ let s = lexeme lexbuf in STRING(String.sub s 1 ((String.length s) - 2)) }

	(* Math Operators *)

	| '+' 					{ PLUS }
	| '-' 					{ MINUS }
	| '*' 					{ TIMES }
	| '/' 					{ DIVIDE }
	| '%' 					{ MODULO }
	| "**" 					{ POWER }

	(* Expression Grouping *)

	| '(' 					{ LPAREN }
	| ')' 					{ RPAREN }

	(* Stream Access *)
	
	| '[' 					{ LBRACKET }
	| ']' 					{ RBRACKET }
	| '~' 					{ CURRENT }

	(* Conditionals *)

	| "==" 					{ EQ }
	| "!=" 					{ NEQ }
	| "<=" 					{ LTE }
	| ">=" 					{ GTE }
	| '<' 					{ LT }
	| '>' 					{ GT }

	(* Control Structures *)

	| "if" 					{ IF }
	| "then" 				{ THEN }
	| "else" 				{ ELSE }
	| "endif" 				{ ENDIF }

	(* Keywords *)

	| "with"				{ USING }
	| "begin"				{ BEGIN }
	| "loop" 				{ LOOP }
	| "skip"                { SKIP }
	| "in"					{ IN }
	| "out" 				{ OUT }

	(* Conditionals *)

	| "and" 				{ AND }
	| "or" 					{ OR }
	
	(* Identifiers *)

	| ['a'-'z' 'A'-'Z' '_'] alphanum* 	{ IDENT(lexeme lexbuf) }

	(* Assignment *)

	| '=' 					{ ASSIGN }
	| "+=" 					{ PLUSASSIGN }
	| "-=" 					{ MINUSASSIGN }
	| "*=" 					{ TIMESASSIGN }
	| "/=" 					{ DIVIDEASSIGN }
	| "++" 					{ INCREMENT }
	| "--" 					{ DECREMENT }

	(* Function Application Expression Lists *)

	| ',' 					{ COMMA }

	(* Scoped Function Application *)

	| '.' 					{ DOT }

	(* Error Reporting *) 

	| _ 					{ lexing_error ("Unrecognized character: " ^ (lexeme lexbuf)) (lexeme_start_p lexbuf) (lexeme_end_p lexbuf) }
	