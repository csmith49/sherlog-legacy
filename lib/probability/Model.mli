type t

(* construction *)
val empty : t
val of_formula : Core.Formula.t -> t

(* display *)
type view = (Data.Identifier.t * Data.Identifier.t list * Distribution.t) -> string
val factor_view : view

val print : view -> t -> unit