type result = CALL.tag list * CALL.t

(* parsing *)

let split_lines lines =
    let handle = function
        | `T t -> `Left t
        | `C c -> `Right c
        | _ -> `Drop in
    CCList.partition_map handle lines

let parse str = str
    |> Lexing.from_string
    |> Parser.program Lexer.read
    |> split_lines

(* extracting *)
let program result = result
    |> snd
    |> CALL.compile

let select_tags (selector : CALL.tag -> 'a option) (result : result) : 'a list = result
    |> fst
    |> CCList.filter_map selector

let queries result =
    let selector = function
        | `Query q -> Some (q
            |> CCList.map CALL.compile_atom
            |> Core.Query.of_atom_list)
        | _ -> None in
    select_tags selector result

let evidence result =
    let selector = function
        | `Evidence o -> Some (o
            |> CCList.map CALL.compile_atom
            |> Core.Evidence.of_atom_list)
        | _ -> None in
    select_tags selector result

let parameters result =
    let selector = function
        | `Parameter p -> Some (p |> Core.Parameter.of_string)
        | _ -> None in
    select_tags selector result