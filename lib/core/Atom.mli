module Symbol : sig
    type t
    
    val to_string : t -> string
    val compare : t -> t -> int
    val equal : t -> t -> bool
end

type t

(* access *)
val symbol : t -> Symbol.t
val arguments : t -> Term.t list
val variables : t -> Data.Identifier.t list
val is_ground : t -> bool

(* printing *)
val to_string : t -> string
val to_json : t -> Yojson.Basic.t

(* equality *)
val equal : t -> t -> bool

(* manipulation *)
val unify : t -> t -> Substitution.t option
val substitute : t -> Substitution.t -> t

(* construction *)
val make : string -> Term.t list -> t

(* positions *)
module Position : sig
    type t = Symbol.t * int

    val compare : t -> t -> int
    val equal : t -> t -> bool
end

val positions : t -> (Data.Identifier.t * Position.t) list