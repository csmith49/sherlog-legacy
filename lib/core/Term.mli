type t =
    | Variable of Data.Identifier.t
    | Integer of int
    | Boolean of bool
    | Atom of string

val to_string : t -> string
val equal : t -> t -> bool

val occurs : Data.Identifier.t -> t -> bool