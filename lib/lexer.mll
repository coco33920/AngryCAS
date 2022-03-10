{
  open Lexing
  type token = 
  | INT of int
  | FLOAT of float 
  | VAR of string
  | PLUS
  | MINUS
  | MULT 
  | LPAR
  | RPAR
  | EQUAL 
  | BOOL of bool
  | NULL
  | EOF

let read_int buffer = 
  try let a = int_of_string buffer in INT(a) 
  with _ -> NULL
let read_float buffer = 
  try let a = float_of_string buffer in FLOAT(a)
  with _ -> NULL

let print_token = function
  | INT i -> Printf.sprintf "{Int : %d}" i
  | FLOAT f -> Printf.sprintf "{Float : %f}" f
  | VAR x -> Printf.sprintf "{Var : %s}" x
  | PLUS -> "{+}"
  | MINUS -> "{-}"
  | MULT -> "{*}"
  | LPAR -> "{(}"
  | RPAR -> "{)}"
  | EQUAL -> "="
  | _ -> ""

let print_token_list lst = 
  Printf.sprintf "[%s]" (String.concat " " (List.map print_token lst)) 


exception SyntaxError of string
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

  

(*reads a float/int token*)
(*let read_token buffer = 
  match buffer with
    | "+" -> PLUS 
    | "(" -> LPAR
    | ")" -> RPAR
    | "=" -> EQUAL
    | s -> try INT(int_of_string s) with _ -> try FLOAT(float_of_string s) with _ -> NULL

let generate_token_with_chars str = 
  let char_list = List.of_seq (String.to_seq str) in
  let rec generate_token acc storage lst = 
    match lst with
      | [] -> List.rev acc 
      | t::q when t=' ' -> generate_token acc storage q
      | t::q -> let storage = storage ^ (String.make 1 t) in 
        match (read_token storage) with
          | NULL -> generate_token acc storage q
          | t -> generate_token (t::acc) "" q
  in generate_token [] "" char_list;;**)
}
let int = '-'? ['0'-'9'] ['0'-'9']*
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let var = ['a'-'z' 'A'-'Z' '_']

rule read = parse
  | white {read lexbuf}
  | newline {next_line lexbuf; read lexbuf}
  | int {INT (int_of_string (Lexing.lexeme lexbuf))} 
  | float {FLOAT (float_of_string (Lexing.lexeme lexbuf))}
  | var {VAR (Lexing.lexeme lexbuf)}
  | "(" {LPAR}
  | ")" {RPAR}
  | "=" {EQUAL}
  | "-" {MINUS}
  | "true" {BOOL true}
  | "false" {BOOL false}
  | "+" {PLUS} 
  | "*" {MULT}
  | _ {raise (SyntaxError ("error while lexing" ^ (Lexing.lexeme lexbuf)))}
  | eof {EOF}