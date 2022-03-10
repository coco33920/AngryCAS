module CAS = struct
  type value = [
    `Int of int | `Float of float | `Var of string | `PLUS
    ]

  open Core
  open Out_channel
  let output_value = function
  | `Int i -> printf "{Int %d}" i
  | `Float x -> printf "{Float %f}" x
  | `Var x -> printf " {Var %s} " x
  | `PLUS -> print_string " {+} "
end  