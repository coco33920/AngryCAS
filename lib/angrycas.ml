module AngryCAS = struct
module Lexer = Lexer
module CAS = Cas.CAS
module Parser = Parser.Parser

let lex_to_list lexbuf =
  let rec aux acc lexbuf = 
    match (Lexer.read lexbuf) with
      | Lexer.EOF -> List.rev acc
      | t -> aux (t::acc) lexbuf
  in aux [] lexbuf 
 
  let string_to_parsed string =
    let lexbuf = Lexing.from_string string in
    let lexed = lex_to_list lexbuf in 
    let parsed = Parser.parse_operation lexed
  in parsed;;


end