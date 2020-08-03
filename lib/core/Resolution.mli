type t = {
    subobligation : Predicate.t;
    substitution : Substitution.t;
    existential_context : Predicate.t list;
}
type resolution = t

val extend : t -> t -> t
val initial : t

module Result : sig
    type t =
        | Success
        | Failure

    val is_successful : t -> bool
end

module Path : sig
    type t
    val terminal_resolution : t -> resolution option
end

module Tree : sig
    type t

    val of_query : Predicate.t list -> t
    val resolve : (Obligation.t -> (resolution * Obligation.t) list) -> t -> t
    val solutions : t -> Substitution.t list
end