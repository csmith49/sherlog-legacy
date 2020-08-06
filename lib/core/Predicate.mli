type symbol
type t

(* access *)
val symbol : t -> symbol
val arguments : t -> Term.t list
val event_space : t -> Term.t list
val variables : t -> Data.Identifier.t list

(* printing *)
val symbol_to_string : symbol -> string
val to_string : t -> string

(* equality *)
val symbol_equal : symbol -> symbol -> bool
val equal : t -> t -> bool

(* manipulation *)
val unify : t -> t -> Substitution.t option
val substitute : t -> Substitution.t -> t

(* construction *)
val make : string -> Term.t list -> t
val make_meta : string -> Term.t list -> Term.t list -> t