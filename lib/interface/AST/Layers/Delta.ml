(*  *)

type delta_clause = [
    | `DeltaClause of delta_atom * CALL.atom list
]
and delta_atom = [
    | `DeltaAtom of string * CALL.term list * distribution
]
and distribution = [
    | `DeltaDistribution of string * CALL.term list * CALL.term list
]

let simplify = function `DeltaClause (`DeltaAtom (name, arguments, `DeltaDistribution (d, params, context)), body) ->
    let variable = "Y" in
    let cost = `Sample (variable, d, params, context) in
    let head = `Atom (name, arguments @ [`Variable variable] ) in
        `C (`Clause (cost, head, body))