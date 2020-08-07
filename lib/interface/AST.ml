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
        | `Atom of string
        | `Function of string * term list
    ]
    and cost = [
        | `Conjunction of conjunct list
    ]
    and conjunct = [
        | `Sim of string * string * term list
    ]

    let rec compile_term : term -> Core.Term.t = function
        | `Variable x -> Core.Term.Make.var x
        | `Integer i -> Core.Term.Make.int i
        | `Boolean b -> Core.Term.Make.bool b
        | `Float f -> Core.Term.Make.float f
        | `Atom s -> Core.Term.Make.atom s
        | `Function (f, args) -> 
            let args = CCList.map compile_term args in
                Core.Term.Make.apply f args
    let compile_predicate : predicate -> Core.Predicate.t = function
        | `Predicate (name, args) ->
            let args = CCList.map compile_term args in
                Core.Predicate.make name args
    let compile_conjunct : conjunct -> Core.Formula.t = function
        | `Sim (var, dist, args) ->
            let var = Core.Term.Make.var var in
            let args = CCList.map compile_term args in
            let term = Core.Term.Make.apply dist args in
                Core.Formula.draw_from var term
    let compile_cost : cost -> Core.Formula.t = function
        | `Conjunction conjuncts -> conjuncts
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
        |> CCPair.map1 Core.Query.of_predicate_list
        |> CCPair.map2 Core.Program.of_clause_list
end