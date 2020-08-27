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

    val compile : t -> (Core.Query.t * Core.Program.t)
end

module Extended : sig
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

    val lift_term : term -> delta_term

    val simplify : t -> Basic.t
end