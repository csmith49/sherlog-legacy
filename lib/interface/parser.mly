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
%token IMPLIES
%token AND
%token PERIOD
%token COMMA
%token QMARK
%token EQUAL
%token LEQ
%token GEQ
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
    | d = SYMBOL; LBRACKET; args = terms; RBRACKET { `Distribution (d, args, []) }
    | d = SYMBOL; LBRACKET; args = terms; SEMICOLON; es = terms; RBRACKET { `Distribution (d, args, es) }
    ;
dterms : dts = separated_list(COMMA, dterm) { dts } ;

predicate : s = SYMBOL; LPARENS; ts = terms; RPARENS { `Predicate (s, ts) } ;
predicates : ps = separated_list(COMMA, predicate) { ps } ;

dpredicate : 
    | s = SYMBOL; LPARENS; ts = terms; RPARENS { `DeltaPredicate (s, lift ts) }
    | s = SYMBOL; LPARENS; ts = terms; MID; dts = dterms; RPARENS { `DeltaPredicate (s, (lift ts) @ dts) }
    ;

conjunct :
    | l = term; EQUAL; r = term { `Eq (l, r) }
    | l = term; LEQ; r = term { `LEq (l, r) }
    | l = term; GEQ; r = term { `LEq (r, l) }
    ;

formula : cs = separated_list(AND, conjunct) { `Formula cs } ;

line :
    | fact = dpredicate; PERIOD { `DeltaRule (fact, []) }
    | qs = predicates; QMARK { `Query qs }
    | head = dpredicate; ARROW; body = predicates; PERIOD { `DeltaRule (head, body) }
    | head = predicate; IMPLIES; cost = formula; PERIOD { `LogicalRule (head, cost) }
    ;

program : cs = list(line); EOF { cs } ;
