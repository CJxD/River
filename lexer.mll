{
	open Parser
}

let white_space = [' ' '\t' '\n']
let digit = ['0'-'9']
let int = digit +
let float = (int '.' int) | (int ['f' 'F'])
let char = ''' [^ '\n'] '''
let bool = "true" | "false"
let alphanum = ['a'-'z' '0'-'9' '_']*

rule token = parse
	
	(* Lines & Whitespace *)

	  white_space 			{ token lexbuf }
	| eof 					{ EOF }
	| ';' 					{ EOL }

	(* Comments *)

	| "//" [^ '\n']* 		{ token lexbuf }
	
	(* | "/*" _* "*/" 			{ token lexbuf } *)

	(* Literals *)

	| int 					{ INT(int_of_string (Lexing.lexeme lexbuf)) }
	| float 				{ FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
	| char 					{ CHAR(String.get (Lexing.lexeme lexbuf) 1) }
	| bool 					{ BOOL(bool_of_string (Lexing.lexeme lexbuf)) }
	| "true" 				{ TRUE }
	| "false" 				{ FALSE }

	(* Operators *)

	| '+' 					{ PLUS }
	| '-' 					{ MINUS }
	| '*' 					{ TIMES }
	| '/' 					{ DIVIDE }
	| '%' 					{ MODULO }
	| "**" 					{ POWER }

	| '(' 					{ LPAREN }
	| ')' 					{ RPAREN }
	| ':' 					{ POINTER }
	| '[' 					{ LBRACKET }
	| ']' 					{ RBRACKET }
	| '=' 					{ ASSIGN }
	| '>' 					{ ANGLERIGHT }
	| '<' 					{ ANGLELEFT }
	| '^' 					{ XOR }
	| '&' 					{ AND }
	| '|' 					{ OR }
	| ',' 					{ COMMA } 
	| "++" 					{ INCREMENT }
	| "--" 					{ DECREMENT }
	| "==" 					{ EQ }
	| ">=" 					{ GTE }
	| "<=" 					{ LTE }
	| "!=" 					{ NEQ }

	(* Keywords *)

	| "if" 					{ IF }
	| "then" 				{ THEN }
	| "else" 				{ ELSE }
	| "with"				{ USING }
	| "begin"				{ BEGIN }
	| "skip"                { SKIP }
	| "loop" 				{ LOOP }
	| "out" 				{ OUT }
	| "in"					{ IN }

	(* Identifiers *)

	| ['a'-'z'] alphanum* 	{ IDENT(Lexing.lexeme lexbuf) }
