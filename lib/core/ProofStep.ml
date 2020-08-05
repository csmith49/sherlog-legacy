type t = {
    cost : Formula.t;
    subobligation : Predicate.t;
    substitution : Substitution.t;
}

let make_free pred sub = {
    cost = Formula.empty;
    subobligation = pred;
    substitution = sub;
}

let make pred sub cost = { (make_free pred sub) with cost = cost}

let initial = make_free (Predicate.make "root" []) Substitution.empty

type sequence = t list

let cost sequence = sequence
    |> CCList.map (fun step -> Formula.substitute step.cost step.substitution)
    |> CCList.fold_left Formula.conjoin Formula.empty