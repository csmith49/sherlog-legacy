module Basic = struct
    type t = line list
    and line = [
        | `Rule of cost * predicate * predicate list
        | `Query of predicate list
    ]
    and predicate = [
        | `Predicate of string * term list
    ]
    and term = [
        | `Variable of string
        | `Integer of int
        | `Boolean of bool
        | `Float of float
        | `Constant of string
        | `Function of string * term list
    ]
    and cost = [
        | `Formula of conjunct list
    ]
    and conjunct = [
        | `Sample of string * string * term list * term list
        | `Eq of term * term
        | `LEq of term * term
    ]

    let rec compile_term : term -> Core.Term.t = function
        | `Variable x -> Core.Term.Make.var x
        | `Integer i -> Core.Term.Make.int i
        | `Boolean b -> Core.Term.Make.bool b
        | `Float f -> Core.Term.Make.float f
        | `Constant s -> Core.Term.Make.const s
        | `Function (f, args) -> 
            let args = CCList.map compile_term args in
                Core.Term.Make.apply f args

    let compile_predicate : predicate -> Core.Atom.t = function
        | `Predicate (name, args) ->
            let args = CCList.map compile_term args in
                Core.Atom.make name args

    let compile_conjunct : conjunct -> Core.Formula.t = function
        | `Sample (var, dist, args, event_space) ->
            let var = Core.Term.Make.var var in
            let args = CCList.map compile_term args in
            let event_space = CCList.map compile_term event_space in
            let dist = Core.Term.Make.apply dist args in
                Core.Formula.Make.draw_from var dist event_space
        | `Eq (left, right) ->
            let left = compile_term left in
            let right = compile_term right in
                Core.Formula.Make.eq left right
        | `LEq (left, right) ->
            let left = compile_term left in
            let right = compile_term right in
                Core.Formula.Make.leq left right

    let compile_cost : cost -> Core.Formula.t = function
        | `Formula conjuncts -> conjuncts
            |> CCList.map compile_conjunct 
            |> CCList.fold_left Core.Formula.conjoin Core.Formula.empty

    let compile_line = function
        | `Rule (cost, head, body) ->
            let cost = compile_cost cost in
            let head = compile_predicate head in
            let body = CCList.map compile_predicate body in
                `Right (Core.Clause.make head body |> Core.Clause.add_cost cost)
        | `Query preds -> 
            let preds = CCList.map compile_predicate preds in
                `Left (preds)

    let compile lines = lines
        |> CCList.partition_map compile_line
        |> CCPair.map1 CCList.flatten
        |> CCPair.map1 Core.Query.of_atom_list
        |> CCPair.map2 Core.Program.of_clause_list
end

module Extended = struct
    type t = line list
    and line = [
        | `DeltaRule of delta_predicate * predicate list
        | `LogicalRule of predicate * cost
        | `Query of predicate list
    ]
    and delta_predicate = [
        | `DeltaPredicate of string * delta_term list
    ]
    and delta_term = [
        | `Variable of string
        | `Integer of int
        | `Boolean of bool
        | `Float of float
        | `Constant of string
        | `Function of string * term list
        | `Distribution of string * term list * term list
    ]
    and predicate = [
        | `Predicate of string * term list
    ]
    and term = [
        | `Variable of string
        | `Integer of int
        | `Boolean of bool
        | `Float of float
        | `Constant of string
        | `Function of string * term list
    ]
    and cost = [
        | `Formula of conjunct list
    ]
    and conjunct = [
        | `Sample of string * string * term list * term list
        | `Eq of term * term
        | `LEq of term * term
    ]

    let lift_term = function
        | (`Variable _ | `Integer _ | `Boolean _ | `Float _ | `Constant _ | `Function _ ) as t -> t

    let simplify_delta_rule = function
        | `DeltaRule (head, body) -> begin match head with
            | `DeltaPredicate (name, arguments) ->
                let arguments', cost = CCList.mapi (fun i -> fun tm -> match tm with
                    | (`Variable _ | `Integer _ | `Boolean _ | `Float _ | `Constant _ | `Function _) as t -> (t, None)
                    | `Distribution (d, p, q) ->
                        let variable = "Sample_" ^ (string_of_int i) in
                        let cost = `Sample (variable, d, p, q) in
                            (`Variable variable, Some cost)
                ) arguments 
                    |> CCList.split
                    |> CCPair.map2 CCList.keep_some in
                let head' = `Predicate (name, arguments') in
                    [ `Rule (`Formula cost, head', body) ]
            end

    let simplify_logical_rule = function
        | `LogicalRule (pred, cost) -> [ `Rule (cost, pred, []) ]

    let simplify_line : line -> Basic.line list = function
        | (`DeltaRule _) as line -> simplify_delta_rule line
        | (`LogicalRule _) as line -> simplify_logical_rule line
        | (`Query _) as line -> [ line ]
    let simplify lines = CCList.flat_map simplify_line lines
end