type t

val to_atom_list : t -> Atom.t list
val of_atom_list : Atom.t list -> t

val to_string : t -> string

val substitute : t -> Substitution.t -> t

val variables : t -> Data.Identifier.t list

val summarize_substitution : t -> Substitution.t -> Substitution.t

val to_json : t -> Yojson.Basic.t
val of_json : Yojson.Basic.t -> t option