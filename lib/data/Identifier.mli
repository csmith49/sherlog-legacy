type context
type t

val name : t -> string
val index : t -> int
val context : t -> context

val to_string : t -> string
val of_string : string -> t

val extend : t -> string -> t
val extend_by_index : t -> int -> t
val split : int -> t -> t list

val equal : t -> t -> bool
val compare : t -> t -> int