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

    let to_json = function
        | Sample (x, dist, es) -> `Assoc [
            ("type", `String "sample");
            ("target", Term.to_json x);
            ("distribution", Term.to_json dist);
            ("event_space", `List (CCList.map Term.to_json es));
        ]
        | Eq (l, r) -> `Assoc [
            ("type", `String "eq");
            ("left", Term.to_json l);
            ("right", Term.to_json r);
        ]
        | LEq (l, r) -> `Assoc [
            ("type", `String "leq");
            ("left", Term.to_json l);
            ("right", Term.to_json r);
        ]

    let of_json json = match Data.JSON.Parse.(find string "type" json) with
        | Some "sample" ->
            let target = Data.JSON.Parse.(find Term.of_json "target" json) in
            let distribution = Data.JSON.Parse.(find Term.of_json "distribution" json) in
            let event_space = Data.JSON.Parse.(find (list Term.of_json) "event_space" json) in
            begin match target, distribution, event_space with
                | Some target, Some distribution, Some event_space -> Some (
                    Sample (target, distribution, event_space)
                )
                | _ -> None end
        | Some "eq" ->
            let left = Data.JSON.Parse.(find Term.of_json "left" json) in
            let right = Data.JSON.Parse.(find Term.of_json "right" json) in
            begin match left, right with
                | Some left, Some right -> Some (Eq (left, right))
                | _ -> None end
        | Some "leq" ->
            let left = Data.JSON.Parse.(find Term.of_json "left" json) in
            let right = Data.JSON.Parse.(find Term.of_json "right" json) in
            begin match left, right with
                | Some left, Some right -> Some (LEq (left, right))
                | _ -> None end
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

(* JSON *)
let to_json formula = `List (CCList.map Conjunct.to_json formula)
let of_json json = Data.JSON.Parse.(list Conjunct.of_json json)