type symbol = {
    name : string;
    arity : int;
}

let symbol_equal left right = (CCString.equal left.name right.name) && (CCInt.equal left.arity right.arity)
let symbol_to_string symbol = symbol.name ^ "/" ^ (string_of_int symbol.arity)

type t = {
    symbol : symbol;
    arguments : Term.t list;
}

let symbol pred = pred.symbol
let arguments pred = pred.arguments

let equal left right = (symbol_equal left.symbol right.symbol) &&
    (CCList.equal Term.equal left.arguments right.arguments)

let to_string predicate =
    let symbol = predicate.symbol.name in
    let args = predicate.arguments |> CCList.map Term.to_string |> CCString.concat ", " in
        symbol ^ "(" ^ args ^ ")"

let unify left right = 
    if not (symbol_equal left.symbol right.symbol) then None else
    let equations = CCList.combine left.arguments right.arguments in
        Unification.solve equations

let substitute pred sub = {pred with
    arguments = pred.arguments |> CCList.map (fun tm -> Substitution.apply tm sub)
}

let make name args =
    let symbol = {
        name = name;
        arity = CCList.length args;
    } in {
        symbol = symbol;
        arguments = args;
    }