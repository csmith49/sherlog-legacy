type t

val of_query : Query.t -> t
val substitute : t -> Substitution.t -> t
val add : Atom.t -> t -> t
val add_all : Atom.t list -> t -> t
val discharge_atom : t -> (Atom.t * t) option
val is_satisfied : t -> bool

val empty : t

val join : t -> t -> t

val to_string : t -> string