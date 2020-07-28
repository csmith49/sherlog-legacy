val var : string -> Term.t
val int : int -> Term.t
val bool : bool -> Term.t
val atom : string -> Term.t

val (/.) : string -> int -> Basic.predicate_symbol
val (:=) : Basic.predicate -> Basic.predicate list -> Basic.clause