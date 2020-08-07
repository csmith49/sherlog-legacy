type t = {
    obligation : Obligation.t;
    existential_context : Predicate.t list;
}

let obligation state = state.obligation
let existential_context state = state.existential_context

let extend_existential_context predicate state = {
    state with existential_context = predicate :: state.existential_context
}
let extend_obligation obligation state = {
    state with obligation = Obligation.join obligation state.obligation
}

let set_obligation obligation state = {
    state with obligation = obligation;
}

let initial = {
    obligation = [] |> Query.of_predicate_list |> Obligation.of_query;
    existential_context = [];
}
let of_query query = initial |> set_obligation (Obligation.of_query query)

let discharge_predicate state = match Obligation.discharge_predicate state.obligation with
    | Some (predicate, obligation) -> Some (predicate, state |> set_obligation obligation)
    | _ -> None

let unify_with_existential_context predicate state = CCList.filter_map (fun ec -> Predicate.unify predicate ec) state.existential_context

let cached_step predicate state = state.existential_context
    |> CCList.filter_map (fun ec -> Predicate.unify predicate ec)
    |> CCList.map (ProofStep.make_free predicate)
    |> CCList.map (fun step -> (step, state))

let to_verbose_string state = [
    "---< P STATE >---";
    "\tObl.: " ^ (state.obligation |> Obligation.to_string);
    "\tECtx: " ^ (state.existential_context |> CCList.map Predicate.to_string |> CCString.concat " | ");
    "-----------------";
] |> CCString.concat "\n"