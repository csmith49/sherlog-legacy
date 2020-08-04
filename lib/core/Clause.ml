type t = {
    head : Predicate.t;
    body : Predicate.t list;
}

let head clause = clause.head
let body clause = clause.body
let variables clause =
    let head_vars = Predicate.variables clause.head in
    let body_vars = CCList.flat_map Predicate.variables clause.body in
        head_vars @ body_vars

let to_string clause =
    let head = Predicate.to_string clause.head in
    let body = clause.body |> CCList.map Predicate.to_string |> CCString.concat ", " in
    if CCString.equal body "" then head else head ^ " :- " ^ body

let substitute clause substitution = {
    head = Predicate.substitute clause.head substitution;
    body = clause.body |> CCList.map (fun p -> Predicate.substitute p substitution);
}
  
let freshen_count = ref 1
  
let freshen clause =
    let index = !freshen_count in
    let sub = clause
        |> variables
        |> CCList.map (fun v ->
            let v' = Data.Identifier.extend_by_index v index in
            (v, Term.Variable v')
          )
        |> Substitution.of_list in
    let _ = freshen_count := !freshen_count + 1 in
    substitute clause sub

let apply obligation clause = match Obligation.discharge_predicate obligation with
    | None -> Some obligation
    | Some (predicate, obligation) -> begin match Predicate.unify predicate clause.head with
        | Some substitution -> obligation
            |> Obligation.add_all clause.body
            |> fun ob -> Obligation.substitute ob substitution
            |> CCOpt.return
        | None -> None
    end

let resolve obligation clause = let clause = freshen clause in match Obligation.discharge_predicate obligation with
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

module IdSet = CCSet.Make(Data.Identifier)

let existential_variables clause =
    let h_vars = clause.head |> Predicate.variables |> IdSet.of_list in
    let b_vars = clause.body |> CCList.flat_map Predicate.variables |> IdSet.of_list in
    let e_vars = IdSet.diff h_vars b_vars in
        e_vars |> IdSet.to_list

(* todo - cache this *)
let is_existential clause = CCList.is_empty (existential_variables clause)