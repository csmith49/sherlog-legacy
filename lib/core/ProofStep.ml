type t = {
    cost : Formula.t;
    subobligation : Predicate.t;
    substitution : Substitution.t;
}
type step = t

let make_free pred sub = {
    cost = Formula.empty;
    subobligation = pred;
    substitution = sub;
}

let make pred sub cost = { (make_free pred sub) with cost = cost}

let initial = make_free (Predicate.make "root" []) Substitution.empty

let to_verbose_string step = [
    "---< P STEP  >---";
    "\tCost: " ^ (step.cost |> Formula.to_string);
    "\tObl.: " ^ (step.subobligation |> Predicate.to_string);
    "\tSub.: " ^ (step.substitution |> Substitution.to_string);
    "-----------------";
] |> CCString.concat "\n"

module Derivation = struct
    type t = step list

    let of_step_list ls = ls
    let to_step_list ls = ls

    let summary der = der
        |> to_step_list
        |> CCList.map (fun step -> 
            let sub = step.substitution in
            let cost = Formula.substitute step.cost sub in
                (cost, sub)
        ) |> CCList.fold_left (fun (c, s) -> fun (c', s') ->
            let cost = Formula.conjoin c c' in
            let sub = Substitution.compose s s' in
                (cost, sub)
        ) (Formula.empty, Substitution.empty)

    let cost der = der |> summary |> fst
end