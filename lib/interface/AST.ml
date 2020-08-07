module Basic = struct
    type t = line list
    and line = [
        | `Rule of predicate * predicate list
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

    let rec compile_term = function
        | `Variable x -> Core.Term.Make.var x
        | `Integer i -> Core.Term.Make.int i
        | `Boolean b -> Core.Term.Make.bool b
        | `Float f -> Core.Term.Make.float f
        | `Atom s -> Core.Term.Make.atom s
        | `Function (f, args) -> 
            let args = CCList.map compile_term args in
                Core.Term.Make.apply f args
    let compile_predicate = function
        | `Predicate (name, args) ->
            let args = CCList.map compile_term args in
                Core.Predicate.make name args
    let compile_line = function
        | `Rule (head, body) -> 
            let head = compile_predicate head in
            let body = CCList.map compile_predicate body in
                `Right (Core.Clause.make head body)
        | `Query preds -> 
            let preds = CCList.map compile_predicate preds in
                `Left (preds)
    let compile lines = lines
        |> CCList.partition_map compile_line
        |> CCPair.map1 CCList.flatten
        |> CCPair.map1 Core.Query.of_predicate_list
        |> CCPair.map2 Core.Program.of_clause_list
end