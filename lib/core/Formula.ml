module Conjunct = struct
    type t = 
        | Sample of (Term.t * Term.t * Term.t list)
        | Eq of (Term.t * Term.t)
        | LEq of (Term.t * Term.t)

    let to_string = function
        | Sample (x, dist, event_space) ->
            let x' = Term.to_string x in
            let dist' = Term.to_string dist in
            let event_space' = event_space
                |> CCList.map Term.to_string
                |> CCString.concat ", " in
            "(" ^ x' ^ " ~ " ^ dist' ^ " | " ^ event_space' ^ ")"
        | Eq (l, r) ->
            let l' = Term.to_string l in
            let r' = Term.to_string r in
            "(" ^ l' ^ " == " ^ r' ^ ")"
        | LEq (l, r) ->
            let l' = Term.to_string l in
            let r' = Term.to_string r in
            "(" ^ l' ^ " <= " ^ r' ^ ")"

    let equal left right = match left, right with
        | Sample (x, dist, event_space), Sample (x', dist', event_space') ->
            (Term.equal x x') &&
            (Term.equal dist dist') &&
            (CCList.equal Term.equal event_space event_space')
        | Eq (l, r), Eq (l', r') ->
            (Term.equal l l') &&
            (Term.equal r r')
        | LEq (l, r), LEq (l', r') ->
            (Term.equal l l') &&
            (Term.equal r r')
        | _ -> false

    let substitute c sub = match c with
        | Sample (x, dist, event_space) ->
            let x' = Substitution.apply x sub in
            let dist' = Substitution.apply dist sub in
            let event_space' = event_space
                |> CCList.map (fun tm -> Substitution.apply tm sub) in
            Sample (x', dist', event_space')
        | Eq (l, r) ->
            let l' = Substitution.apply l sub in
            let r' = Substitution.apply r sub in
            Eq (l', r')
        | LEq (l, r) ->
            let l' = Substitution.apply l sub in
            let r' = Substitution.apply r sub in
            LEq (l', r')

    let variables = function
        | Sample (x, dist, event_space) ->
            CCList.flat_map Term.variables (x :: dist :: event_space)
        | Eq (l, r) -> (Term.variables l) @ (Term.variables r)
        | LEq (l, r) -> (Term.variables l) @ (Term.variables r)

    let decompose_sample = function
        | Sample (x, dist, event_space) -> Some (x, dist, event_space)
        | _ -> None
end

type t = Conjunct.t list

let empty = []
let is_empty = CCList.is_empty
let conjuncts ls = ls

let to_string formula = if CCList.is_empty formula then "true" else formula
    |> CCList.map Conjunct.to_string
    |> CCString.concat " ∧ "

let simplify = CCList.uniq ~eq:Conjunct.equal

let conjoin = (@)

let substitute formula sub = formula
    |> CCList.map (fun c -> Conjunct.substitute c sub)

let variables = CCList.flat_map Conjunct.variables

module Make = struct
    let draw_from x dist es = [Conjunct.Sample (x, dist, es)]
    let eq x y = [Conjunct.Eq (x, y)]
    let leq x y = [Conjunct.LEq (x, y)]
end