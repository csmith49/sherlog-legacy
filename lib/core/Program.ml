type t = Clause.t list

let of_clause_list p = p
let to_clause_list p = p

let rec to_string = function
    | [] -> ""
    | clause :: rest ->
        let clause' = Clause.to_string clause in
        let rest' = to_string rest in
            clause' ^ "\n" ^ rest'

let resolve obligation program = program |> CCList.filter_map (Clause.apply obligation)

let linear_strategy program state = program
    |> to_clause_list
    |> CCList.flat_map (Clause.resolve state)