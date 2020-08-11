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
    let body = if CCList.is_empty clause.body then "" else
        let preds = clause.body |> CCList.map Predicate.to_string |> CCString.concat ", " in
            " :- " ^ preds in
    let cost = if Formula.is_empty clause.cost then "" else
        (Formula.to_string clause.cost) ^ " : " in
    cost ^ head ^ body

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
    match state |> Proof.State.discharge_predicate with
        | None -> []
        | Some (predicate, state) ->
            begin match Predicate.unify predicate clause.head with
                | Some substitution ->
                    (* check if the result is to be cached *)
                    let result = Predicate.substitute predicate substitution in
                    let state = if Predicate.is_ground result then
                        Proof.State.extend_cache result state
                    else state in
                    (* derive next step *)
                    let step = Proof.Step.make predicate substitution clause.cost in
                    (* update obligation *)
                    let state = state
                        |> Proof.State.extend_obligation clause.body
                        |> fun s -> Proof.State.substitute s substitution in
                    [(step, state)]
                | None -> []
            end

let make head body = {head = head; body = body; cost = Formula.empty}
let fact pred = {head = pred; body = []; cost = Formula.empty}

let add_cost cost clause = {clause with cost = Formula.conjoin cost clause.cost}
