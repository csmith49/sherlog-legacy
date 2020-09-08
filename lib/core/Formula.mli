module Conjunct : sig
    type t

    val to_string : t -> string
    val equal : t -> t -> bool
    val substitute : t -> Substitution.t -> t

    val variables : t -> Data.Identifier.t list

    val decompose_sample : t -> (Term.t * Term.t * Term.t list) option
end

type t

(* construction *)
val empty : t

(* checks *)
val is_empty : t -> bool

(* printing *)
val to_string : t -> string

(* manipulation *)
val simplify : t -> t
val conjoin : t -> t -> t
val conjuncts : t -> Conjunct.t list
val substitute : t -> Substitution.t -> t
val variables : t -> Data.Identifier.t list

(* JSON manipulation *)
val to_json : t -> Yojson.Basic.t
val of_json : Yojson.Basic.t -> t option

(* utilities for construction costs *)
module Make : sig
    val draw_from : Term.t -> Term.t -> Term.t list -> t
    val eq : Term.t -> Term.t -> t
    val leq : Term.t -> Term.t -> t
end
