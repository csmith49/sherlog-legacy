type t = Atom.t list

let variables q = q |> CCList.flat_map Atom.variables |> CCList.uniq ~eq:Data.Identifier.equal

let of_atom_list ps = ps
let to_atom_list ps = ps

let substitute q sub = q
    |> CCList.map (fun p -> Atom.substitute p sub)

let to_string q = let q' = q
    |> CCList.map Atom.to_string
    |> CCString.concat ", " in
        q' ^ "?"

let summarize_substitution q sub = q
    |> variables
    |> CCList.map (fun x -> (x, Substitution.lookup_identifier x sub))
    |> Substitution.of_list

let to_json query = query
    |> to_atom_list
    |> CCList.map Atom.to_json
    |> Data.JSON.Make.list
let of_json json = json
    |> Data.JSON.Parse.list Atom.of_json
    |> CCOpt.map of_atom_list