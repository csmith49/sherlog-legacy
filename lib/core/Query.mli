type t

val to_predicate_list : t -> Predicate.t list
val of_predicate_list : Predicate.t list -> t

val to_string : t -> string

val substitute : t -> Substitution.t -> t

val variables : t -> Data.Identifier.t list

val summarize_substitution : t -> Substitution.t -> Substitution.t