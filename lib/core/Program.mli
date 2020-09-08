(* basic aliases *)
type t
type program = t

(* conversion from clauses *)
val of_clause_list : Clause.t list -> t
val to_clause_list : t -> Clause.t list

(* printing *)
val to_string : t -> string

(* the simplest strateguy *)
val linear_strategy : t -> Proof.strategy

(* modify obligations *)
val resolve : Obligation.t -> t -> Obligation.t list

(* for determining weak acyclicity *)
module DependencyGraph : sig
    type t

    val of_program : program -> t
end

(* JSON *)
val to_json : t -> Yojson.Basic.t
val of_json : Yojson.Basic.t -> t option