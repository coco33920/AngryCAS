module CAS = struct
include Parser.Parser

  type numbers = INT of int | FLOAT of float 
  type var = Atom of string | Vars of numbers * var
  
  (*1 degree implementation of a multi-variable polynomial structure*)
  (*TODO: a better structure*)
  type monome = Monome of var | SUM of monome * monome
  type expr = 
    | Number of numbers 
    | BOOL of bool 
    | Variable of var 
    | PLUS of expr * expr 
    | MULT of expr * expr 
    | EQUAL of expr * expr 
    | MINUS of expr * expr
    | NULL


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

  let is_zero a = 
    match a with
      | INT(i) -> i=0
      | FLOAT(f) -> f=0.

  let reduce_nested_var var = 
    let rec aux acc var = 
      match var with 
        | Atom x -> Vars(acc,Atom(x))
        | Vars(l,x) -> aux (mult_reals l acc) x
    in aux (INT 1) var;;

  (*WARNING: all variables must be reduced*)
  let rec add_var_to_monome var monome =
    match monome with
      | Monome(Atom(x)) -> 
        (match var with
          | Atom(y) when String.equal x y -> Monome(Vars(INT(2),Atom(x)))
          | Atom(y) -> SUM(Monome(Atom(y)),Monome(Atom(x)))
          | Vars(a,Atom(y)) when String.equal x y -> Monome(Vars(add_reals a (INT(1)), Atom(x)))
          | Vars(a,Atom(y)) -> SUM(Monome(Atom(x)),Monome(Vars(a,Atom(y))))
          | _ -> Monome(Atom(x)))
      | Monome(Vars(a,Atom(x))) ->
        (match var with
          | Atom(y) when String.equal x y -> Monome(Vars(add_reals a (INT(1)), Atom(x)))
          | Atom(y) -> SUM(Monome(Vars(a,Atom(x))),Monome(Atom(y)))
          | Vars(b,Atom(y)) when String.equal x y -> Monome(Vars(add_reals a b,Atom(y)))
          | Vars(b,Atom(y)) -> SUM(Monome(Vars(a,Atom(x))),Monome(Vars(b,Atom(y))))
          | _ -> Monome(Atom(x))
          )
      | Monome(Vars(a,x)) -> Monome(Vars(a,x))
        
      | SUM(m1,m2) -> SUM(add_var_to_monome var m1,add_var_to_monome var m2)
  
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

  let sub_vars a b = match reduce_nested_var a,reduce_nested_var b with
      | Atom(x),Atom(y) when String.equal x y -> Variable(Vars(INT(0),Atom(x)))
      | Vars(l,Atom(x)),Vars(u,Atom(y)) when String.equal x y -> let u = mult_reals (INT(-1)) u in Variable(Vars(add_reals l u, Atom(x)))
      | e,f -> MINUS(Variable(e),Variable(f))

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
  
  
  let oppose_reals a = mult_reals a (INT(-1))
  let print_expr expr =
    let rec pretty_print_expr expr =
    match expr with 
      | Number(a) -> print_string (print_numbers a)
      | BOOL b -> print_string (string_of_bool b)
      | Variable(v) -> print_string (print_var v)
      | PLUS(a,b) -> print_string "("; pretty_print_expr a; print_string "+"; pretty_print_expr b; print_string ")"
      | MULT(a,b) -> print_string "("; pretty_print_expr a; print_string "*"; pretty_print_expr b; print_string ")"
      | EQUAL(a,b) -> print_string "("; pretty_print_expr a; print_string "="; pretty_print_expr b; print_string ")"
      | MINUS(a,b) -> print_string "("; pretty_print_expr a; print_string "-"; pretty_print_expr b; print_string ")"
      
      | NULL -> print_string "ɛ"
    in pretty_print_expr expr;;

  let rec add a b = 
    print_expr a;
    print_string " + ";
    print_expr b;
    print_newline ();
    match a,b with 
      (*basic expr*)
      | Number a,NULL -> Number a
      | Number a,Number b -> Number(add_reals a b)
      | Number _,BOOL _ -> BOOL (false)
      | Number a,Variable v -> PLUS(Number a,Variable v)
      | Variable v,Number a -> PLUS(Number a,Variable v)
      | Variable v,NULL -> Variable v
      | Variable(v),Variable(v') -> add_vars v v'
      | Variable _,BOOL _ -> BOOL(false)
      | BOOL b,NULL -> BOOL b
      | BOOL _,Number _ -> BOOL(false)
      | BOOL _,Variable _ -> BOOL(false)
      | BOOL b,BOOL c -> BOOL(c || b)
      | NULL,Number(a) -> Number a 
      | NULL,Variable v -> Variable v
      | NULL,BOOL b -> BOOL b 
      | NULL,NULL -> NULL

      (*PLUS expr*)
      | PLUS(a,b),PLUS(c,d) -> add (add a b) (add c d)
      | PLUS(a,b),MULT(c,d) -> add (add a b) (mult c d)
      | PLUS(_,_),MINUS(_,_) -> NULL
      | PLUS(_,_),EQUAL(_,_) -> BOOL(false)

      (*PLUS atom*)
      | PLUS(a,b),NULL -> add a b
      | PLUS(a,b),c -> add (add a b) c
      | MULT(a,b),PLUS(c,d) -> add (mult a b) (add c d)
      | a,PLUS(b,c) -> add a (add b c)
      
      (*MULT expr*)
      | MULT(a,b),NULL -> mult a b
      | MULT(a,b),MULT(c,d) -> add (mult a b) (mult c d)
      | MULT(_,_),MINUS(_,_) -> NULL
      | MULT(_,_),EQUAL(_,_) -> BOOL(false)

      (*MULT atom*)
      | MULT(a,b),c -> add (mult a b) c
      | a,MULT(b,c) -> add a (mult b c)

      | _,MINUS(_,_) -> NULL
      | _,EQUAL(_,_) -> BOOL(false)
      | MINUS(_,_),_ -> NULL
      | EQUAL(_,_),_ -> BOOL(false)


  and mult a b = 
    print_expr a;
    print_string " * ";
    print_expr b;
    print_newline ();
    match a,b with
      (*basic expr*)
      | Number a, NULL -> Number(a)
      | Number(a),Number(b) -> Number(mult_reals a b)
      | Number(a),Variable(v) -> let b = (Vars(a,v)) in Variable(reduce_nested_var b)
      | Number _,BOOL _ -> BOOL (false)
      | Variable v,NULL -> Variable v
      | Variable v,Number a -> let b = (Vars(a,v)) in Variable(reduce_nested_var b)
      | Variable(v),Variable(v') -> mult_vars v v'
      | Variable _,BOOL _ -> BOOL(false)
      | BOOL b,NULL -> BOOL(b)
      | BOOL _,Number _ -> BOOL(false)
      | BOOL _,Variable _ -> BOOL(false)
      | BOOL b,BOOL c -> BOOL(c && b)
      | NULL,Number(a) -> Number a 
      | NULL,Variable v -> Variable v
      | NULL,BOOL b -> BOOL b 
      | NULL,NULL -> NULL

      (*PLUS expr*)
      | PLUS(a,b),PLUS(c,d) -> add (add (mult a c) (mult a d)) (add (mult b c) (mult b d)) (*double distributivité*)
      | PLUS(a,b),MULT(c,d) -> let cd = mult c d in add (mult a cd) (mult b cd)
      | PLUS(_,_),MINUS(_,_) -> NULL
      | PLUS(_,_),EQUAL(_,_) -> BOOL(false)

      (*PLUS atom*)
      | PLUS(a,b),NULL -> add a b
      | PLUS(a,b),c -> add (mult a c) (mult b c) (*distributivité simple*)
      | MULT(a,b),PLUS(c,d) -> let ab = mult a b in add (mult ab c) (mult ab d)
      | a,PLUS(b,c) -> add (mult a b) (mult a c)

      (*MULT expr*)
      | MULT(a,b),NULL -> mult a b
      | MULT(a,b),MULT(c,d) -> mult (mult a b) (mult c d)
      | MULT(_,_),MINUS(_,_) -> NULL
      | MULT(_,_),EQUAL(_,_) -> BOOL(false)

      (*MULT atom*)
      | MULT(a,b),c -> mult (mult a b) c
      | a,MULT(b,c) -> mult a (mult b c)

      | _,MINUS(_,_) -> NULL
      | _,EQUAL(_,_) -> BOOL(false)
      | MINUS(_,_),_ -> NULL
      | EQUAL(_,_),_ -> BOOL(false)

  let equal (_:expr) (_:expr) = NULL
  let minus (_:expr) (_:expr) = NULL

  let oppose_expr expr = mult (Number(INT(-1))) expr

  let rec reduce_expression expr = 
     match expr with 
      | NULL -> NULL
      | Number(a) -> Number(a)
      | BOOL b -> BOOL b
      | Variable(v) -> Variable(reduce_nested_var v)
      | PLUS(a,b) ->  let a,b = reduce_expression a,reduce_expression b in add a b
      | MULT(a,b) -> let a,b = reduce_expression a,reduce_expression b in mult a b
      | EQUAL(a,b) -> let a,b=reduce_expression a,reduce_expression b in equal a b
      | MINUS(a,b) -> let a,b=reduce_expression a,reduce_expression b in minus a b

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
      | Node(Expression(INFIX MINUS),g,d) -> MINUS(transform_ast_to_expression g, transform_ast_to_expression d)

  let reduce_ast ast = transform_ast_to_expression ast |> reduce_expression;;


  let test = EQUAL(PLUS(Variable(Vars (INT 3,Atom("x"))),Number(INT 1)),PLUS(Variable(Vars (INT 3,Atom("x"))),Number(INT 1)));;
  let test2 = MINUS(PLUS(Variable(Vars (INT 3,Atom("x"))),Number(INT 1)),PLUS(Variable(Vars (INT 3,Atom("x"))),Number(INT 1)));;
  let test3 = MULT(Number(INT (-1)),Variable(Vars(INT 3, Atom("x"))))
  let i () = test |> print_expr; print_newline ();;
  let f () = test2 |> reduce_expression |> print_expr; print_newline ();;
end  