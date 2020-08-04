type t

val obligation : t -> Obligation.t
val existential_context : t -> Predicate.t list

val initial : t
val of_query : Predicate.t list -> t

val extend_existential_context : Predicate.t -> t -> t
val set_obligation : Obligation.t -> t -> t