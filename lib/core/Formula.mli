module Conjunct : sig
    type t

    val to_string : t -> string
    val equal : t -> t -> bool
    val substitute : t -> Substitution.t -> t
end

type t

val empty : t
val is_empty : t -> bool
val to_string : t -> string
val simplify : t -> t
val conjoin : t -> t -> t
val conjuncts : t -> Conjunct.t list
val substitute : t -> Substitution.t -> t

module Make : sig
    val draw_from : Term.t -> Term.t -> Term.t list -> t
    val eq : Term.t -> Term.t -> t
    val leq : Term.t -> Term.t -> t
end
