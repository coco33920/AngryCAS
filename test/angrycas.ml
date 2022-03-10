open Angrycas.AngryCAS
let str = "3=3";;
print_endline str;;
let lexbuf = Lexing.from_string str;;
let list = lex_to_list lexbuf;;
let parsed = Parser.parse_operation list;;
let _ = print_endline (Parser.print_ast parsed);;

let param = CAS.reduce_ast parsed in CAS.print_expr param;;