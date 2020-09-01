(* main type - models are graphs under the hood *)
type t

(* interface types *)
type node = Data.Identifier.t
type view = node * node list * Distribution.t
type observation = (node * Core.Term.t) list

(* access *)
val nodes : t -> node list
val parents : node -> t -> node list
val cpd : node -> t -> Distribution.t

val views : t -> view list
val observations : t -> observation list

(* construction *)
val empty : t
val add_view : view -> t -> t
val add_observation : observation -> t -> t

val of_proof : Core.Formula.t list -> t

(* display *)
val view_to_factor_string : view -> string
val print : (view -> string) -> t -> unit

(* and writing out *)
val to_json : t -> Yojson.Basic.t