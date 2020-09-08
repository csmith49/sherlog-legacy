let parse_ast string = Some (Parser.program Lexer.read (Lexing.from_string string))

let parse_file filename =
    let file_contents = filename
        |> open_in
        |> (fun f -> really_input_string f (in_channel_length f)) in
    file_contents
        |> parse_ast
        |> CCOpt.map AST.Extended.simplify
        |> CCOpt.map AST.Basic.compile

let parse_string str = str
    |> parse_ast
    |> CCOpt.map AST.Extended.simplify
    |> CCOpt.map AST.Basic.compile

module Network = struct
    type handler = Network.handler
    type port = int

    let local_handler_server port handler =
        let socket = Network.socket Network.local_address port in
        let server = Network.server handler socket in
            server
end