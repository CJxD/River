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
	| '"' [^ '"']* '"'		{ let s = Lexing.lexeme lexbuf in STRING(String.sub s 1 ((String.length s) - 2)) }

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

	| ['a'-'z'] alphanum* 	{ IDENT(Lexing.lexeme lexbuf) }

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

	(* Unused *)

	| '^' 					{ XOR }
	| '&' 					{ AND }
	| '|' 					{ OR }
	| '~' 					{ NOT }
	