%{
    let lift = CCList.map AST.Extended.lift_term
%}

%token LPARENS
%token RPARENS
%token LBRACKET
%token RBRACKET
%token SEMICOLON
%token MID
%token ARROW
%token PERIOD
%token COMMA
%token QMARK
%token EOF

%token TRUE
%token FALSE
%token <float> FLOAT
%token <int> INTEGER
%token <string> SYMBOL
%token <string> VARIABLE

%start <AST.Extended.t> program

%%

term :
    | TRUE { `Boolean true }
    | FALSE { `Boolean false }
    | f = FLOAT { `Float f }
    | i = INTEGER { `Integer i} 
    | s = SYMBOL { `Atom s }
    | x = VARIABLE { `Variable x }
    | f = SYMBOL; LPARENS; args = separated_list(COMMA, term); RPARENS { `Function (f, args) }
    ;
terms : ts = separated_list(COMMA, term) { ts } ;

dterm : 
    | TRUE { `Boolean true }
    | FALSE { `Boolean false }
    | f = FLOAT { `Float f }
    | i = INTEGER { `Integer i} 
    | s = SYMBOL { `Atom s }
    | x = VARIABLE { `Variable x }
    | f = SYMBOL; LPARENS; args = separated_list(COMMA, term); RPARENS { `Function (f, args) }
    | d = SYMBOL; LBRACKET; args = terms; RBRACKET { `Sample (d, args, []) }
    | d = SYMBOL; LBRACKET; args = terms; SEMICOLON; es = terms; RBRACKET { `Sample (d, args, es) }
    ;
dterms : dts = separated_list(COMMA, dterm) { dts } ;

predicate : s = SYMBOL; LPARENS; ts = terms; RPARENS { `Predicate (s, ts) } ;
predicates : ps = separated_list(COMMA, predicate) { ps } ;

dpredicate : 
    | s = SYMBOL; LPARENS; ts = terms; RPARENS { `DeltaPredicate (s, lift ts) }
    | s = SYMBOL; LPARENS; ts = terms; MID; dts = dterms; RPARENS { `DeltaPredicate (s, (lift ts) @ dts) }
    ;
    
line :
    | fact = dpredicate; PERIOD { `DeltaRule (fact, []) }
    | qs = predicates; QMARK { `Query qs }
    | head = dpredicate; ARROW; body = predicates; PERIOD { `DeltaRule (head, body) }
    ;

program : cs = list(line); EOF { cs } ;
