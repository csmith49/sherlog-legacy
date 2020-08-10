{
    open Parser
}

let whitespace = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read = parse
    | whitespace { read lexbuf }
    | "(" { LPARENS }
    | ")" { RPARENS }
    | "[" { LBRACKET }
    | "]" { RBRACKET }
    | ";" { SEMICOLON }
    | ":-" { ARROW }
    | "<-" { IMPLIES }
    | "&" { AND }
    | "|" { MID }
    | "." { PERIOD }
    | "," { COMMA }
    | "?" { QMARK }
    | "==" { EQUAL }
    | "<=" { LEQ }
    | ">=" { GEQ }
    | eof { EOF }
    | "true" { TRUE }
    | "false" { FALSE }
    | '-'? ['0'-'9']+ '.' ['0'-'9']+ { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | '-'? ['0'-'9']+ { INTEGER (int_of_string (Lexing.lexeme lexbuf)) }
    | ['A'-'Z'][''']? { VARIABLE (Lexing.lexeme lexbuf)}
    | ['a'-'z']['a'-'z' '_' '0'-'9']* { SYMBOL (Lexing.lexeme lexbuf) }