type t = Predicate.t list

let of_query q = q

let substitute ob sub = ob
    |> CCList.map (fun p -> Predicate.substitute p sub)

let add pred ob = pred :: ob
let add_all preds ob = ob @ preds

let discharge_predicate = function
    | [] -> None
    | pred :: rest -> Some (pred, rest)

let is_predicate_satisfied = function
    | [] -> true
    | _ -> false

let to_string ob = ob
    |> CCList.map Predicate.to_string
    |> CCString.concat ", "