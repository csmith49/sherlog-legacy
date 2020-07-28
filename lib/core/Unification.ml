type equation = Term.t * Term.t

(* implementation of Martelli-Montanari unification *)
let rec solve equations = solve_aux equations Substitution.empty
and solve_aux equations sub = match equations with
    | [] -> Some sub
    | (x, y) :: rest when Term.equal x y -> solve_aux rest sub
    | (Term.Variable x, y) :: rest ->
        if Term.occurs x y then None else
        let sub' = Substitution.add x y sub in
        let rest' = CCList.map 
            (fun (l, r) -> (Substitution.apply l sub', Substitution.apply r sub'))
            rest in
        solve_aux rest' sub'
    | (x, Term.Variable y) :: rest ->
        if Term.occurs y x then None else
        let sub' = Substitution.add y x sub in
        let rest' = CCList.map 
            (fun (l, r) -> (Substitution.apply l sub', Substitution.apply r sub'))
            rest in
        solve_aux rest' sub'
    | _ -> None