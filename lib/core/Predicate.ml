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
let variables pred = pred.arguments
    |> CCList.filter_map (fun arg -> match arg with
        | Term.Variable x -> Some x
        | _ -> None
    )

let equal left right = (symbol_equal left.symbol right.symbol) &&
    (CCList.equal Term.equal left.arguments right.arguments)

let to_string predicate =
    let symbol = predicate.symbol.name in
    let arguments' = predicate.arguments |> CCList.map Term.to_string |> CCString.concat ", " in
    symbol ^ "(" ^ arguments' ^ ")"

let unify left right = 
    if not (symbol_equal left.symbol right.symbol) then None else
    let argument_eqs = CCList.combine left.arguments right.arguments in
    let equations = argument_eqs in
        Unification.solve equations

let substitute pred sub = {pred with
    arguments = pred.arguments |> CCList.map (fun tm -> Substitution.apply tm sub);
}

let make name args =
    let symbol = {
        name = name;
        arity = CCList.length args;
    } in {
        symbol = symbol;
        arguments = args;
    }
