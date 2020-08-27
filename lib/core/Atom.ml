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

let symbol atom = atom.symbol
let arguments atom = atom.arguments
let variables atom = atom.arguments
    |> CCList.filter_map (fun arg -> match arg with
        | Term.Variable x -> Some x
        | _ -> None
    )
let is_ground atom = atom.arguments
    |> CCList.for_all Term.is_ground

let equal left right = (symbol_equal left.symbol right.symbol) &&
    (CCList.equal Term.equal left.arguments right.arguments)

let to_string atom =
    let symbol = atom.symbol.name in
    let arguments' = atom.arguments |> CCList.map Term.to_string |> CCString.concat ", " in
    symbol ^ "(" ^ arguments' ^ ")"

let unify left right = 
    if not (symbol_equal left.symbol right.symbol) then None else
    let argument_eqs = CCList.combine left.arguments right.arguments in
    let equations = argument_eqs in
        Unification.solve equations

let substitute atom sub = {atom with
    arguments = atom.arguments |> CCList.map (fun tm -> Substitution.apply tm sub);
}

let make name args =
    let symbol = {
        name = name;
        arity = CCList.length args;
    } in {
        symbol = symbol;
        arguments = args;
    }
