module CAS = struct
  type expression = [
    `PLUS of expression * expression
  ]
  type value = [
    `Int of int | `Float of float | `Var of string | `Expression of expression
  ]
  

  open Core
  open Out_channel
  let output_value = function
  | `Int i -> printf "{Int %d}" i
  | `Float x -> printf "{Float %f}" x
  | `Var x -> printf " {Var %s} " x
  | `Expression _ -> print_string " {+} "
end  