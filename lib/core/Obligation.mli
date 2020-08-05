type t

val of_query : Predicate.t list -> t
val substitute : t -> Substitution.t -> t
val add : Predicate.t -> t -> t
val add_all : Predicate.t list -> t -> t
val discharge_predicate : t -> (Predicate.t * t) option
val is_predicate_satisfied : t -> bool

val empty : t

val join : t -> t -> t

val to_string : t -> string