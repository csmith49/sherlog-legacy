type t = {
    cost : Formula.t;
    subobligation : Predicate.t;
    substitution : Substitution.t;
}
type step = t

val make_free : Predicate.t -> Substitution.t -> t
val make : Predicate.t -> Substitution.t -> Formula.t -> t
val initial : t

val to_verbose_string : t -> string

module Derivation : sig
    type t

    val of_step_list : step list -> t
    val cost : t -> Formula.t
    val summary : t -> (Formula.t * Substitution.t)
end