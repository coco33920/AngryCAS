let str = "x + 2."
let lexbuf = Lexing.from_string str
let _ = Angrycas.AngryCAS.parse_and_print lexbuf;;