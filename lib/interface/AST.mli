module Basic : sig
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

    val compile : t -> (Core.Query.t * Core.Program.t)
end