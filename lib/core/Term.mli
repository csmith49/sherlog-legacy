(* Simple, first-order terms *)
type t =
    | Variable of Data.Identifier.t
    | Integer of int
    | Float of float
    | Boolean of bool
    | Constant of string
    | Function of string * t list

val to_string : t -> string
val equal : t -> t -> bool

val occurs : Data.Identifier.t -> t -> bool

val is_ground : t -> bool

val variables : t -> Data.Identifier.t list

val to_json : t -> Yojson.Basic.t

module Make : sig
    val var : string -> t
    val int : int -> t
    val float : float -> t
    val bool : bool -> t
    val const : string -> t
    val apply : string -> t list -> t
end