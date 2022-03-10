open Angrycas.AngryCAS
let () = print_endline "entrez une formule"
let () = print_newline();;
let str = read_line ();;
let _ = string_to_parsed str  |> CAS.reduce_ast |> CAS.print_expr