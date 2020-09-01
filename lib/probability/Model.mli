type t

val of_list : (Data.Identifier.t * Data.Identifier.t list * Distribution.t) list -> t
val to_list : t -> (Data.Identifier.t * Data.Identifier.t list * Distribution.t) list

val to_json : t -> Yojson.Basic.t

val of_formula : Core.Formula.t -> t option