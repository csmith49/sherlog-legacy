type conjunct = Sim of Term.t * Term.t

let conjunct_to_string = function
    | Sim (x, d) ->
        let x' = Term.to_string x in
        let d' = Term.to_string d in
            x' ^ " ~ " ^ d'

type t = conjunct list

let to_string formula = if CCList.is_empty formula then "true" else formula
    |> CCList.map conjunct_to_string
    |> CCString.concat " & "

let empty = []

let conjoin = (@)

let draw_from x d = [Sim (x, d)]

let conjuncts x = x

let substitute formula sub = formula
    |> CCList.map (fun (Sim (x, y)) -> Sim (Substitution.apply x sub, Substitution.apply y sub))