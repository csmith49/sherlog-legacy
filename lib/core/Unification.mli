type equation = Term.t * Term.t

val solve : equation list -> Substitution.t option