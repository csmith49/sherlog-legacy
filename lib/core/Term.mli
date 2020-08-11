(* Simple, first-order terms *)
type t =
    | Variable of Data.Identifier.t
    | Integer of int
    | Float of float
    | Boolean of bool
    | Atom of string
    | Function of string * t list

val to_string : t -> string
val equal : t -> t -> bool

val occurs : Data.Identifier.t -> t -> bool

val is_ground : t -> bool

module Make : sig
    val var : string -> t
    val int : int -> t
    val float : float -> t
    val bool : bool -> t
    val atom : string -> t
    val apply : string -> t list -> t
end