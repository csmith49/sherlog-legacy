val var : string -> Term.t
val int : int -> Term.t
val bool : bool -> Term.t
val atom : string -> Term.t

val (:=) : Predicate.t -> Predicate.t list -> Clause.t