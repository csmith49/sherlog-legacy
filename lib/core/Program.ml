type t = Clause.t list

let of_clause_list p = p
let to_clause_list p = p

let rec to_string = function
  | [] -> ""
  | clause :: rest ->
    let clause' = Clause.to_string clause in
    let rest' = to_string rest in
      clause' ^ "\n" ^ rest'