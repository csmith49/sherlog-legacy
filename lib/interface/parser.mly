%{
%}

%token LPARENS
%token RPARENS
%token LBRACKET
%token RBRACKET
%token SEMICOLON
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

%start <AST.Basic.t> program

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

term_list : ts = separated_list(COMMA, term) { ts } ;

predicate : s = SYMBOL; LPARENS; ts = term_list; RPARENS { `Predicate (s, ts) } ;

predicate_list : ps = separated_list(COMMA, predicate) { ps } ;

line :
    | fact = predicate; PERIOD { `Rule (fact, []) }
    | head = predicate; ARROW; body = predicate_list; PERIOD { `Rule (head, body) }
    | qs = predicate_list; QMARK { `Query qs }
    ;

program : cs = list(line); EOF { cs } ;
