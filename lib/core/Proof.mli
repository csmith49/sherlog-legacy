module State : sig
    type t

    val obligation : t -> Obligation.t
    val extend_obligation : Atom.t list -> t -> t
    val set_obligation : Obligation.t -> t -> t

    val cache : t -> Atom.t list
    val extend_cache : Atom.t -> t -> t

    val substitute : t -> Substitution.t -> t

    val initial : t
    val of_query : Query.t -> t
    
    val discharge_atom : t -> (Atom.t * t) option
end

module Step : sig
    type t
    type step = t

    val make : Atom.t -> Substitution.t -> Formula.t -> t
    val initial : t

    module Derivation : sig
        type t

        val to_step_list : t -> step list
        val of_step_list : step list -> t

        val summary : t -> (Formula.t * Substitution.t)

        val cost : t -> Formula.t
    end
end

type strategy = State.t -> (Step.t * State.t) list

module Tree : sig
    type node
    type t

    val of_query : Query.t -> t
    val resolve : strategy -> t -> t
    val solutions : t -> Step.Derivation.t list
end