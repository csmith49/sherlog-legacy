type conjunct

val conjunct_to_string : conjunct -> string

type t

val to_string : t -> string
val empty : t

val conjoin : t -> t -> t

val draw_from : Term.t -> Term.t -> t
val conjuncts : t -> conjunct list

val substitute : t -> Substitution.t -> t