type t = Clause.t list

let of_clause_list p = p
let to_clause_list p = p

let to_string program = program
    |> to_clause_list
    |> CCList.map Clause.to_string
    |> CCList.map (fun cs -> cs ^ ".")
    |> CCString.concat "\n"

let resolve obligation program = program |> CCList.filter_map (Clause.apply obligation)

let try_cache state = match Proof.State.discharge_predicate state with
    | Some (predicate, state) ->
        if CCList.exists (fun p -> Predicate.equal p predicate) (Proof.State.cache state) then
            Some state
        else None
    | None -> None

let linear_strategy program state = match try_cache state with
    | Some state -> [ (Proof.Step.initial, state) ]
    | None -> program
        |> to_clause_list
        |> CCList.flat_map (Clause.resolve state)