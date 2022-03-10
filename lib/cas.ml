module CAS = struct
include Parser.Parser

  type numbers = INT of int | FLOAT of float 
  type var = Atom of string | Vars of numbers * var
  type expr = Number of numbers | BOOL of bool | Variable of var | PLUS of expr * expr | MULT of expr * expr | EQUAL of expr * expr | NULL



  let add_reals a b = 
    match a,b with
      | INT(i),INT(i') -> INT(i+i')
      | INT(i),FLOAT(f) -> FLOAT(float_of_int i +. f)
      | FLOAT(f),INT(i) -> FLOAT(float_of_int i +. f)
      | FLOAT(f),FLOAT(f') -> FLOAT(f +. f')
  let mult_reals a b = 
    match a,b with
      | INT(i),INT(i') -> INT(i * i')
      | INT(i),FLOAT(f) -> FLOAT(float_of_int i *. f)
      | FLOAT(f),INT(i) -> FLOAT(float_of_int i *. f)
      | FLOAT(f),FLOAT(f') -> FLOAT(f *. f')
  
  let equal_number a b =
    match a,b with
      | INT(i),INT(i') -> BOOL(i=i')
      | INT(i),FLOAT(f) -> BOOL((float_of_int i)=f)
      | FLOAT(f),INT(i) -> BOOL((float_of_int i)=f)
      | FLOAT(f),FLOAT(f') -> BOOL(f=f')

  let reduce_nested_var var = 
    let rec aux acc var = 
      match var with 
        | Atom x -> Vars(acc,Atom(x))
        | Vars(l,x) -> aux (mult_reals l acc) x
    in aux (INT 1) var;;
  
  let equal_var a b = 
    match (reduce_nested_var a,reduce_nested_var b) with 
      | Atom(x),Atom(y) -> BOOL(String.equal x y)
      | Vars(l,Atom(x)),Vars(u,Atom(y)) -> BOOL(l=u && String.equal x y)
      | _ -> BOOL(false)

  let add_vars a b = 
    match reduce_nested_var(a),reduce_nested_var(b) with 
      | Atom(x),Atom(y) when String.equal x y -> Variable(Vars(INT(2),Atom(x)))
      | Vars(l,Atom(x)),Vars(u,Atom(y)) when String.equal x y -> Variable(Vars(add_reals l u, Atom(x)))
      | e,f -> PLUS(Variable(e),Variable(f))

  let mult_vars a b = 
    match reduce_nested_var(a),reduce_nested_var(b) with 
      | Atom(x),Atom(y) -> Variable(Atom(x^y))
      | Vars(l,Atom(x)),Vars(u,Atom(y)) -> Variable(Vars(mult_reals l u, Atom(x^y)))
      | e,f -> MULT(Variable e, Variable f)

  let print_numbers = function
    | (INT i) -> Printf.sprintf "%d" i
    | (FLOAT f) -> Printf.sprintf "%f" f


  let print_var var = 
    match (reduce_nested_var var) with
      | Atom x -> x
      | Vars(l,Atom(x)) -> Printf.sprintf "%s%s" (print_numbers l) x
      | _ -> ""

  let rec add param1 param2 = match param1,param2 with
    | Number(a),Number(b) -> Number(add_reals a b)
    | Number(a),Variable(x) -> PLUS(Number(a),Variable(x))
    | Variable(x),Variable(y) -> add_vars x y
    | Variable(x),Number(a) -> PLUS(Number(a),Variable(x))
    | a,PLUS(b,c) -> PLUS(add a b, c) (*associativité*)
    | a,MULT(b,c) -> add a (MULT(b,c)) 
    | PLUS(b,c),a -> PLUS(add b c, a) (*associativité*)
    | MULT(b,c),a -> PLUS(MULT(b,c),a)
    | e,NULL -> e
    | NULL,e -> e
    | _ -> NULL
  let rec mult param1 param2 = 
    match param1,param2 with
    | Number(a),Number(b) -> Number(mult_reals a b)
    | Number(a),Variable(x) -> let v = (Vars(a,x)) in Variable(reduce_nested_var v)
    | Variable(x),Variable(y) -> mult_vars x y
    | Variable(x),Number(a) -> let v = (Vars(a,x)) in Variable(reduce_nested_var v)
    | a,PLUS(b,c) -> add (mult a b) (mult a c) (*distributivité*)
    | a,MULT(b,c) -> MULT(mult a b, c) (*associativité*)
    | PLUS(b,c),a -> add (mult b a) (mult c a) (*distributivité*)
    | MULT(b,c),a -> MULT(MULT(b,c),a) (*associativité*)
    | BOOL b,NULL -> BOOL b
    | NULL,BOOL b -> BOOL b
    | BOOL b, BOOL c -> BOOL (c && b)
    | e,NULL -> e
    | NULL,e -> e
    | _ -> NULL

  let equal param1 param2 = 
    match param1,param2 with
      | Number(a),Number(b) -> equal_number a b
      | Variable x, Variable y -> equal_var x y
      | _,_ -> BOOL(false)



let print_expr expr =
    let rec pretty_print_expr expr =
    match expr with 
      | Number(a) -> print_string (print_numbers a)
      | BOOL b -> print_string (string_of_bool b)
      | Variable(v) -> print_string (print_var v)
      | PLUS(a,b) -> print_string "("; pretty_print_expr a; print_string "+"; pretty_print_expr b; print_string ")"
      | MULT(a,b) -> print_string "("; pretty_print_expr a; print_string "*"; pretty_print_expr b; print_string ")"
      | EQUAL(a,b) -> print_string "("; pretty_print_expr a; print_string "="; pretty_print_expr b; print_string ")"
      | NULL -> print_string "ɛ"
    in pretty_print_expr expr; print_newline ();;


let rec reduce_expression expr = 
    match expr with 
      | NULL -> NULL
      | Number(a) -> Number(a)
      | BOOL b -> BOOL b
      | Variable(v) -> Variable(v)
      | PLUS(a,b) -> let a,b = reduce_expression a,reduce_expression b in add a b
      | MULT(a,b) -> let a,b = reduce_expression a,reduce_expression b in mult a b
      | EQUAL(a,b) -> let a,b = reduce_expression a,reduce_expression b in equal a b

  let rec transform_ast_to_expression ast =
    match ast with 
      | Nil -> NULL
      | Node(Null,_,_) -> NULL
      | Node(Expression(Atom(Int(i))),_,_) -> Number(INT(i))
      | Node(Expression(Atom(Float(f))),_,_) -> Number(FLOAT f)
      | Node(Expression(Atom(Var(x))),_,_) -> Variable(Atom(x))
      | Node(Expression(Atom(Bool b)),_,_) -> BOOL(b)
      | Node(Expression((INFIX(PLUS))),g,d) -> PLUS(transform_ast_to_expression g, transform_ast_to_expression d)
      | Node(Expression((INFIX(MULT))),g,d) -> MULT(transform_ast_to_expression g, transform_ast_to_expression d)
      | Node(Expression((INFIX(EQUAL))), g,d) -> EQUAL(transform_ast_to_expression g, transform_ast_to_expression d)
    

  let reduce_ast ast = transform_ast_to_expression ast |> reduce_expression;;
end  