type t = {
    head : Predicate.t;
    body : Predicate.t list;
    cost : Formula.t; (* by construction, formula only contains variables in head/body *)
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
    cost = Formula.substitute clause.cost substitution;
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

let resolve state clause = let clause = freshen clause in
    match state |> ProofState.discharge_predicate with
        | None -> []
        | Some (predicate, state) ->
            let cached_steps = ProofState.cached_step predicate state in
            begin match Predicate.unify predicate clause.head with
                | Some substitution ->
                    let obligation = state
                        |> ProofState.obligation
                        |> Obligation.add_all clause.body
                        |> fun ob -> Obligation.substitute ob substitution in
                    let step = ProofStep.make predicate substitution clause.cost in
                    (step, state |> ProofState.set_obligation obligation) :: cached_steps
                | None -> []
            end

let make head body = {head = head; body = body; cost = Formula.empty}
let fact pred = {head = pred; body = []; cost = Formula.empty}

let add_cost cost clause = {clause with cost = Formula.conjoin cost clause.cost}
