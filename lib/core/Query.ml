type t = Predicate.t list

let variables q = q |> CCList.flat_map Predicate.variables |> CCList.uniq ~eq:Data.Identifier.equal

let of_predicate_list ps = ps
let to_predicate_list ps = ps

let substitute q sub = q
    |> CCList.map (fun p -> Predicate.substitute p sub)

let to_string q = let q' = q
    |> CCList.map Predicate.to_string
    |> CCString.concat ", " in
        q' ^ "?"

let summarize_substitution q sub = q
    |> variables
    |> CCList.map (fun x -> (x, Substitution.lookup_identifier x sub))
    |> Substitution.of_list