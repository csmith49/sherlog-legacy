type t = Atom.t list

let of_query q = q |> Query.to_atom_list

let substitute ob sub = ob
    |> CCList.map (fun p -> Atom.substitute p sub)

let add pred ob = pred :: ob
let add_all preds ob = ob @ preds

let discharge_atom = function
    | [] -> None
    | pred :: rest -> Some (pred, rest)

let is_satisfied = function
    | [] -> true
    | _ -> false

let empty = []

let join = (@)

let to_string ob = ob
    |> CCList.map Atom.to_string
    |> CCString.concat ", "