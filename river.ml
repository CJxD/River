
let _ =
 try
 let sourcefile = open_in Sys.argv.(0) in
  let lexbuf = Lexing.from_channel sourcefile in
   while true do
     let result = Parser.main Lexer.token lexbuf in
       print_int result; print_newline(); flush stdout
   done
 with Lexer.Eof -> 
  exit 0
