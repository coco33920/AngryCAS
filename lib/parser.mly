%token <int> INT
%token <float> FLOAT
%token <string> VAR
%token PLUS
%token EOF

%start <Cas.CAS.value option> prog
%%

prog: 
    | EOF {None}
    | v = value {Some v}

value:
    | i = INT {`Int i}
    | f = FLOAT {`Float f}
    | v = VAR {`Var v}
    | PLUS {`PLUS}