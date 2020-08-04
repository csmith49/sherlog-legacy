type t

(* access *)
val head : t -> Predicate.t
val body : t -> Predicate.t list
val variables : t -> Data.Identifier.t list
val existential_variables : t -> Data.Identifier.t list
val is_existential : t -> bool

(* printing *)
val to_string : t -> string

(* manipulation *)
val substitute : t -> Substitution.t -> t
val freshen : t -> t

(* usage *)
val apply : Obligation.t -> t -> Obligation.t option
val resolve : Context.t -> t -> (Resolution.t * Context.t) option

(* construction *)
val make : Predicate.t -> Predicate.t list -> t
val fact : Predicate.t -> t