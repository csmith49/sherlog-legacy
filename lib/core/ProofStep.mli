type t = {
    cost : Formula.t;
    subobligation : Predicate.t;
    substitution : Substitution.t;
}

val make_free : Predicate.t -> Substitution.t -> t
val make : Predicate.t -> Substitution.t -> Formula.t -> t
val initial : t

type sequence = t list
val cost : sequence -> Formula.t