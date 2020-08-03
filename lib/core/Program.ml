type t = Clause.t list

let rec to_string = function
  | [] -> ""
  | clause :: rest ->
    let clause' = Clause.to_string clause in
    let rest' = to_string rest in
      clause' ^ "\n" ^ rest'