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
    | ":-" { ARROW }
    | "." { PERIOD }
    | "," { COMMA }
    | "?" { QMARK }
    | eof { EOF }
    | "true" { TRUE }
    | "false" { FALSE }
    | '-'? ['0'-'9']+ { INTEGER (int_of_string (Lexing.lexeme lexbuf)) }
    | ['A'-'Z'] { VARIABLE (Lexing.lexeme lexbuf)}
    | ['a'-'z']+ { SYMBOL (Lexing.lexeme lexbuf) }