module AngryCAS = struct
open Core
open Lexer
open Lexing

module Parser = Parser
module Lexer = Lexer
module CAS = Cas.CAS

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | Syntax msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

  let print_value = CAS.output_value;;

    let rec parse_and_print lexbuf =
      match parse_with_error lexbuf with
      | Some value ->
        Cas.CAS.output_value value;
        parse_and_print lexbuf
      | None -> ()
    
    let loop filename () =
      let inx = In_channel.create filename in
      let lexbuf = Lexing.from_channel inx in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      parse_and_print lexbuf;
      In_channel.close inx
    
    let loops () =
      Command.basic_spec ~summary:"Parse and display JSON"
        Command.Spec.(empty +> anon ("filename" %: string))
        loop
      |> Command.run
  end