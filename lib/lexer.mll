{
    open Lexing
    open Parser
    exception Syntax of string
}

let int = '-'? ['0'-'9'] ['0'-'9']*
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let var = ['a'-'z' 'A'-'Z' '_']*

rule read = parse
    | white {read lexbuf}
    | newline {new_line lexbuf; read lexbuf}
    | int { INT (int_of_string (Lexing.lexeme lexbuf)) } 
    | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | "+" {PLUS}
    | var {VAR (Lexing.lexeme lexbuf)}
    | _ {raise (Syntax ("error: " ^ (Lexing.lexeme lexbuf)))}
    | eof {EOF}