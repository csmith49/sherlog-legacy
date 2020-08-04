type t = {
    obligation : Obligation.t;
    existential_context : Predicate.t list;
}

let obligation context = context.obligation
let existential_context context = context.existential_context

let extend_existential_context predicate context = {
    context with existential_context = predicate :: context.existential_context
}

let set_obligation obligation context = {
    context with obligation = obligation;
}

let initial = {
    obligation = Obligation.of_query [];
    existential_context = [];
}
let of_query query = initial |> set_obligation (Obligation.of_query query)