module Symbol = struct
    type t = {
        name : string;
        arity : int;
    }

    let to_string symbol = symbol.name ^ "/" ^ (string_of_int symbol.arity)
    let compare s1 s2 = 
        let name_comp = CCString.compare s1.name s2.name in
        if name_comp == 0 then
            CCInt.compare s1.arity s2.arity
        else name_comp
    let equal s1 s2 = (compare s1 s2) == 0
end


type t = {
    symbol : Symbol.t;
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

let equal left right = (Symbol.equal left.symbol right.symbol) &&
    (CCList.equal Term.equal left.arguments right.arguments)

let to_string atom =
    let symbol = atom.symbol.name in
    let arguments' = atom.arguments |> CCList.map Term.to_string |> CCString.concat ", " in
    symbol ^ "(" ^ arguments' ^ ")"

let to_json atom = `Assoc [
    ("relation", `String atom.symbol.name);
    ("arguments", `List (atom.arguments |> CCList.map Term.to_json));
]

let unify left right = 
    if not (Symbol.equal left.symbol right.symbol) then None else
    let argument_eqs = CCList.combine left.arguments right.arguments in
    let equations = argument_eqs in
        Unification.solve equations

let substitute atom sub = {atom with
    arguments = atom.arguments |> CCList.map (fun tm -> Substitution.apply tm sub);
}

let make name args =
    let symbol = {
        Symbol.name = name;
        arity = CCList.length args;
    } in {
        symbol = symbol;
        arguments = args;
    }

(* positions *)
module Position = struct
    type t = Symbol.t * int

    let compare = CCPair.compare Symbol.compare CCInt.compare
    let equal = CCPair.equal Symbol.equal CCInt.equal
end

let positions atom = atom.arguments
    |> CCList.mapi (fun index -> fun tm -> match tm with
        | Term.Variable x -> Some (x, (atom.symbol, index))
        | _ -> None)
    |> CCList.keep_some