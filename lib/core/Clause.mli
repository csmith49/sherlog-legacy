type t

(* access *)
val head : t -> Atom.t
val body : t -> Atom.t list
val variables : t -> Data.Identifier.t list

(* printing *)
val to_string : t -> string

(* manipulation *)
val substitute : t -> Substitution.t -> t
val freshen : t -> t

(* usage *)
val apply : Obligation.t -> t -> Obligation.t option
val resolve : Proof.State.t -> t -> (Proof.Step.t * Proof.State.t) list

(* construction *)
val make : Atom.t -> Atom.t list -> t
val fact : Atom.t -> t

val add_cost : Formula.t -> t -> t