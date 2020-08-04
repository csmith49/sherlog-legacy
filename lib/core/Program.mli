type t

val of_clause_list : Clause.t list -> t
val to_clause_list : t -> Clause.t list

val to_string : t -> string

val linear_strategy : t -> Obligation.t -> (Resolution.t * Obligation.t) list

val resolve : Obligation.t -> t -> Obligation.t list