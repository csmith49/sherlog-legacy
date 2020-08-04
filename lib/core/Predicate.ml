type symbol = {
    name : string;
    arity : int;
}

let symbol_equal left right = (CCString.equal left.name right.name) && (CCInt.equal left.arity right.arity)
let symbol_to_string symbol = symbol.name ^ "/" ^ (string_of_int symbol.arity)

type t = {
    symbol : symbol;
    arguments : Term.t list;
    event_space : Term.t list;
}

let symbol pred = pred.symbol
let arguments pred = pred.arguments
let event_space pred = pred.event_space
let variables pred = pred.arguments @ pred.event_space
    |> CCList.filter_map (fun arg -> match arg with
        | Term.Variable x -> Some x
        | _ -> None
    )

let equal left right = (symbol_equal left.symbol right.symbol) &&
    (CCList.equal Term.equal left.arguments right.arguments) &&
    (CCList.equal Term.equal left.event_space right.event_space)

let to_string predicate =
    let symbol = predicate.symbol.name in
    let arguments' = predicate.arguments |> CCList.map Term.to_string |> CCString.concat ", " in
    let event_space' =
      if CCList.is_empty predicate.event_space then
          ""
      else
          "; " ^ (predicate.event_space |> CCList.map Term.to_string |> CCString.concat ", ") in
    symbol ^ "(" ^ arguments' ^ event_space' ^ ")"

let unify left right = 
    if not (symbol_equal left.symbol right.symbol) then None else
    let argument_eqs = CCList.combine left.arguments right.arguments in
    let event_space_eqs = CCList.combine left.event_space right.event_space in
    let equations = argument_eqs @ event_space_eqs in
        Unification.solve equations

let substitute pred sub = {pred with
    arguments = pred.arguments |> CCList.map (fun tm -> Substitution.apply tm sub);
    event_space = pred.event_space |> CCList.map (fun tm -> Substitution.apply tm sub);
}

let make name args =
    let symbol = {
        name = name;
        arity = CCList.length args;
    } in {
        symbol = symbol;
        arguments = args;
        event_space = [];
    }