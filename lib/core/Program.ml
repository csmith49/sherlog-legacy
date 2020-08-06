type t = Clause.t list

let of_clause_list p = p
let to_clause_list p = p

let to_string program = program
    |> to_clause_list
    |> CCList.map Clause.to_string
    |> CCString.concat "\n"

let resolve obligation program = program |> CCList.filter_map (Clause.apply obligation)

let linear_strategy program state = program
    |> to_clause_list
    |> CCList.flat_map (Clause.resolve state)