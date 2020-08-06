type strategy = ProofState.t -> (ProofStep.t * ProofState.t) list

module Result : sig
    type t =
        | Success
        | Failure

    val is_successful : t -> bool
end

module Tree : sig
    type t

    val of_query : Query.t -> t
    val resolve : strategy -> t -> t
    val solutions : t -> ProofStep.Derivation.t list
end