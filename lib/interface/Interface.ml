let parse string = Some (Parser.program Lexer.read (Lexing.from_string string))

let parse_file filename =
  let file_contents = filename
    |> open_in
    |> (fun f -> really_input_string f (in_channel_length f)) in
  parse file_contents