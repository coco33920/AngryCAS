module Parser = struct
  type atom = Int of int | Float of float | Var of string | Bool of bool
  type infix = PLUS | MULT | EQUAL
  type expr = Atom of atom | INFIX of infix
  type parameters = Null | Expression of expr
  type 'a ast = Nil | Node of 'a * 'a ast * 'a ast
  let null_node = Node(Null,Nil,Nil)
  let token_to_ast = function
    | Lexer.INT i -> Node(Expression(Atom(Int(i))),Nil,Nil)
    | Lexer.FLOAT d -> Node(Expression(Atom(Float(d))),Nil,Nil)
    | Lexer.VAR x -> Node(Expression(Atom(Var(x))),Nil,Nil)
    | Lexer.PLUS -> Node(Expression(INFIX(PLUS)),Nil,Nil)
    | Lexer.MULT -> Node(Expression(INFIX(MULT)),Nil,Nil)
    | _ -> null_node

  let token_to_expr = function
    | Lexer.INT i -> Expression(Atom(Int(i)))
    | Lexer.FLOAT f -> Expression(Atom(Float(f)))
    | Lexer.VAR x -> Expression(Atom(Var(x)))
    | Lexer.PLUS -> Expression(INFIX(PLUS))
    | Lexer.MULT -> Expression(INFIX(MULT))
    | Lexer.BOOL b -> Expression(Atom(Bool(b)))
    | _ -> Null 


  let sprint_atom = function 
    | Int i -> Printf.sprintf "{ParsedInt %d}" i 
    | Float f -> Printf.sprintf "{ParsedFloat %f}" f
    | Var x -> Printf.sprintf "{ParsedVar %s}" x
    | Bool b -> Printf.sprintf "{ParsedBool %b}" b

  let sprint_infix = function
    | PLUS -> "{+}" 
    | MULT -> "{*}"
    | EQUAL -> "{=}"

  let sprint_expr = function
    | Atom a -> Printf.sprintf "(Atom: %s)" (sprint_atom a)
    | INFIX i -> Printf.sprintf "(INFIX: %s)" (sprint_infix i)
  let sprint_param = function
    | Expression e -> sprint_expr e
    | Null -> "Null"
  
  let rec print_ast = function
    | Nil -> "Nil"
    | Node(p, g, d) -> Printf.sprintf "(%s[%s;%s])" (sprint_param p) (print_ast g) (print_ast d)
  
  let print_ast_list ast = List.iter (fun c -> print_string (print_ast c); print_newline ()) ast
  

    let parse_operation list_of_tokens =
    let rec aux last_param lst =

       match last_param,lst with
      (*Parenthesis*)
      
      | Nil,Lexer.LPAR::q -> let rest,ast = aux null_node q in 
          let node = ast
          in  aux node rest
      
        (*Addition*)
      | Node(Expression (INFIX(PLUS)),g,Nil),Lexer.LPAR::q -> let rest,ast = aux null_node q
        in let node = Node(Expression (INFIX (PLUS)), g, ast)
        in aux node rest

        (*Implied multiplication*)
      | Node(e,g,d),Lexer.LPAR::q -> let rest,ast = aux null_node q 
        in let node = Node(Expression (INFIX (MULT)),Node(e,g,d),ast)
        in aux node rest
    

      | _,Lexer.RPAR::q -> q,last_param

      (*implied mults*)
        (*atom * var*)
      | Node(Expression(Atom y),Nil,Nil),(Lexer.VAR x)::q ->
        let node = Node(Expression(INFIX(MULT)),Node(Expression (Atom y), Nil, Nil),Node(Expression(Atom(Var(x))), Nil, Nil))
        in aux node q
        (*var * int*)
      | Node(Expression(Atom(Var x)),Nil,Nil),(Lexer.INT i)::q ->
        let node = Node(Expression(INFIX(MULT)),Node(Expression (Atom (Var x)), Nil, Nil),Node(Expression(Atom(Int(i))), Nil, Nil))
        in aux node q
      (*var * float*)
      | Node(Expression(Atom(Var x)),Nil,Nil),(Lexer.FLOAT i)::q ->
          let node = Node(Expression(INFIX(MULT)),Node(Expression (Atom (Var x)), Nil, Nil),Node(Expression(Atom(Float(i))), Nil, Nil))
          in aux node q


      (*Remaining*)
      | Node(Null,_,_),t::q -> aux (Node(token_to_expr t, Nil, Nil)) q
      | Nil,t::q -> aux (Node(token_to_expr t, Nil, Nil)) q
      | Node(e,g,d),Lexer.PLUS::q -> aux (Node(Expression(INFIX(PLUS)), Node(e,g,d),Nil)) q
      | Node(e,g,d),Lexer.MULT::q -> aux (Node(Expression(INFIX(MULT)), Node(e,g,d), Nil)) q
      | Node(e,g,d),Lexer.EQUAL::q -> aux (Node(Expression(INFIX EQUAL),Node(e,g,d),Nil)) q
      | Node(Expression(INFIX(i)),g,Nil),t::q -> aux (Node(Expression (INFIX(i)),g,token_to_ast t)) q
      | Node(Expression(INFIX(i)),Nil,d),t::q -> aux (Node(Expression(INFIX(i)),token_to_ast t,d)) q
      | Node(Expression(INFIX(i)),g,d),t::q -> aux (Node(token_to_expr t, Node(Expression(INFIX(i)),g,d), Nil)) q
      | Node(e,Nil,Nil),t::q -> aux (Node(e,token_to_ast t,Nil)) q
      | Node(e,g,Nil),t::q -> aux (Node(e,g,token_to_ast t)) q
      | Node(e,g,d),t::q -> aux (Node(Expression(INFIX(MULT)), Node(e,g,d), token_to_ast t)) q
      | Node(e,g,d),[] -> [],Node(e,g,d)
      | Nil,[] -> [],last_param
      
  in let _,a = aux null_node list_of_tokens in a;;
  (*() -> el*)

  

end