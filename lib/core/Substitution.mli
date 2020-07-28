type t

(* construction and extension *)
val empty : t
val add : Data.Identifier.t -> Term.t -> t -> t
val add_all : (Data.Identifier.t * Term.t) list -> t -> t
val of_list : (Data.Identifier.t * Term.t) list -> t

(* usage on terms *)
val apply : Term.t -> t -> Term.t

(* printing *)
val to_string : t -> string