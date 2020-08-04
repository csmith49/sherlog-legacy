type t = {
    head : Predicate.t;
    body : Predicate.t list;
}

let head clause = clause.head
let body clause = clause.body

let to_string clause =
    let head = Predicate.to_string clause.head in
    let body = clause.body |> CCList.map Predicate.to_string |> CCString.concat ", " in
    if CCString.equal body "" then head else head ^ " :- " ^ body

let apply obligation clause = match Obligation.discharge_predicate obligation with
    | None -> Some obligation
    | Some (predicate, obligation) -> begin match Predicate.unify predicate clause.head with
        | Some substitution -> obligation
            |> Obligation.add_all clause.body
            |> fun ob -> Obligation.substitute ob substitution
            |> CCOpt.return
        | None -> None
    end

let resolve obligation clause = match Obligation.discharge_predicate obligation with
    | None -> None
    | Some (predicate, obligation) -> begin match Predicate.unify predicate clause.head with
        | Some substitution ->
            let obligation = obligation
                |> Obligation.add_all clause.body
                |> fun ob -> Obligation.substitute ob substitution in
            let resolution = {
                Resolution.subobligation = predicate;
                substitution = substitution;
            } in
            Some (resolution, obligation)
        | _ -> None
    end

let make head body = {head = head; body = body}
let fact pred = {head = pred; body = []}