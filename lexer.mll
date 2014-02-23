{
    open Parser
    exception Eof
}

let digit = ['0'-'9']
let int = digit +
let float = (int '.' int) | (int ['f' 'F'])
let char = [^ '\n']
let bool = "true" | "false"
let alphanum = ['a'-'z' '0'-'9' '_']*

rule token = parse
      [' ' '\t']            { token lexbuf }
    | ['\n']                { EOL }
    | int                   { INT(int_of_string (Lexing.lexeme lexbuf)) }
    | float                 { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
    | char                  { CHAR(char_of_string (Lexing.lexeme lexbuf)) }
    | bool                  { BOOL(bool_of_string (Lexing.lexeme lexbuf)) }
    | "true"                { TRUE }
    | "false"               { FALSE }
    | '+'                   { PLUS }
    | '-'                   { MINUS }
    | '*'                   { TIMES }
    | '/'                   { DIV }
    | '('                   { LPAREN }
    | ')'                   { RPAREN }
    | ':'                   { POINTER }
    | '['                   { LBRACKET }
    | ']'                   { RBRACKET }
    | '='                   { ASSIGN }
    | '>'                   { ANGLERIGHT }
    | '<'                   { ANGLELEFT }
    | '^'                   { XOR }
    | '&'                   { AND }
    | '|'                   { OR }
    | ','                   { COMMA } 
    | "++"                  { INCREMENT }
    | "--"                  { DECREMENT }
    | "->"                  { LAMBDA }
    | "=="                  { EQ }
    | ">="                  { GTE }
    | "<="                  { LTE }
    | "!="                  { NEQ }
    | "if"                  { IF }
    | "then"                { THEN }
    | "else"                { ELSE }
    | "while"               { WHILE }
    | "do"                  { DO }
    | ['a'-'z'] alphanum*   { IDENT(Lexing.lexeme lexbuf) }
    | eof                   { raise Eof }
