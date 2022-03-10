open Angrycas.AngryCAS
let str = "40*y";;
let lexbuf = Lexing.from_string str in
let list = lex_to_list lexbuf in print_endline (Lexer.print_token_list list);
let parsed = Parser.parse_operation list in print_newline (); print_endline (Parser.print_ast parsed);;