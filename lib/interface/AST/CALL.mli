(** {0 AST} *)

(** {1 Representation} *)

type t = clause list
(** lists of clauses *)

and clause = [
    | `Clause of cost * atom * atom list
]
(** constructed in an abducible sense *)

and cost = [
    | `True
    | `Sample of string * string * term list * term list
]
(** to be expanded *)

and term = [
    | `Variable of string
    | `Integer of int
    | `Boolean of bool
    | `Float of float
    | `Constant of string
    | `Function of string * term list
]
(** mirrors core data forms *)

and atom = [
    | `Atom of string * term list
]
(** or, a relation *)

type tag = [
    | `Query of atom list
    | `Parameter of string
    | `Evidence of atom list
]

type line = [
    | `T of tag
    | `C of clause
]
(** for simpler parsing *)

(** {1 Compilation} *)

val compile_term : term -> Core.Term.t
val compile_atom : atom -> Core.Atom.t
val compile : t -> Core.Program.t
(** build a program *)
