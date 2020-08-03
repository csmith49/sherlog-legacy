%{
    open Core
    open Data
%}

%token LPARENS
%token RPARENS
%token ARROW
%token PERIOD
%token COMMA
%token QMARK
%token EOF

%token TRUE
%token FALSE
%token <int> INTEGER
%token <string> SYMBOL
%token <string> VARIABLE

%start <(Predicate.t list * Program.t)> program

%%

term :
    | TRUE { Term.Boolean true }
    | FALSE { Term.Boolean false }
    | i = INTEGER { Term.Integer i} 
    | s = SYMBOL {Term.Atom s }
    | x = VARIABLE { Term.Variable (Identifier.of_string x) }
    ;

term_list : ts = separated_list(COMMA, term) { ts } ;

predicate : s = SYMBOL; LPARENS; ts = term_list; RPARENS { Predicate.make s ts } ;

predicate_list : ps = separated_list(COMMA, predicate) { ps } ;

line :
    | fact = predicate; PERIOD { Source.EDB (Clause.fact fact) }
    | head = predicate; ARROW; body = predicate_list; PERIOD { Source.EDB (Clause.make head body) }
    | qs = predicate_list; QMARK { Source.IDB qs }
    ;

program : cs = list(line); EOF { Source.process_lines cs } ;
