module Symbol : sig
    type t

    val to_string : t -> string
    val of_string : string -> t option
    
    val arguments : t -> string list

    val to_json : t -> Yojson.Basic.t
end

type t

val of_term : Core.Term.t -> t option

val to_json : t -> Yojson.Basic.t

val to_string : t -> string

val dummy : t

val variables : t -> Data.Identifier.t list