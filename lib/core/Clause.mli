type t

(* access *)
val head : t -> Predicate.t
val body : t -> Predicate.t list

(* printing *)
val to_string : t -> string

(* usage *)
val apply : Obligation.t -> t -> Obligation.t option

(* construction *)
val make : Predicate.t -> Predicate.t list -> t
val fact : Predicate.t -> t