type t

val obligation : t -> Obligation.t
val existential_context : t -> Predicate.t list

val initial : t
val of_query : Query.t -> t

val extend_existential_context : Predicate.t -> t -> t
val extend_obligation : Obligation.t -> t -> t
val set_obligation : Obligation.t -> t -> t

val discharge_predicate : t -> (Predicate.t * t) option

val unify_with_existential_context : Predicate.t -> t -> Substitution.t list

(* different kinds of steps *)
val cached_step : Predicate.t -> t -> (ProofStep.t * t) list

val to_verbose_string : t -> string