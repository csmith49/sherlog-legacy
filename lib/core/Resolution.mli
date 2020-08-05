type strategy = State.t -> (ProofStep.t * State.t) list

module Result : sig
    type t =
        | Success
        | Failure

    val is_successful : t -> bool
end

module Tree : sig
    type t

    val of_query : Predicate.t list -> t
    val resolve : strategy -> t -> t
    val solutions : t -> ProofStep.sequence list
end