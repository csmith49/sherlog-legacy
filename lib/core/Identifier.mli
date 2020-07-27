type context
type t

val name : t -> string
val index : t -> int
val context : t -> context

val to_string : t -> string
val of_string : string -> t

val extend : string -> t -> t
val split : int -> t -> t list

val equal : t -> t -> bool
val compare : t -> t -> int