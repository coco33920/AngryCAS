open Angrycas.AngryCAS
let str = "10(10+(10+x))";;
print_endline str;;
let lexbuf = Lexing.from_string str;;
let list = lex_to_list lexbuf;;
let parsed = Parser.parse_operation list;;
let _ = print_endline (Parser.print_ast parsed);;
let transformed = CAS.transform_ast_to_expression parsed;;
let _ = CAS.print_expr transformed;;
let _ = print_newline ();;

(*let _ = CAS.f();;*)

let reduced = CAS.reduce_expression transformed;;
let _ = CAS.print_expr reduced;;
