(* representation *)

type t = clause list
and clause = [
    | `Clause of cost * atom * atom list
]
and cost = [
    | `True
    | `Sample of string * string * term list * term list
]
and term = [
    | `Variable of string
    | `Integer of int
    | `Boolean of bool
    | `Float of float
    | `Constant of string
    | `Function of string * term list
]
and atom = [
    | `Atom of string * term list
]

type tag = [
    | `Query of atom list
    | `Parameter of string
    | `Evidence of atom list
]

type line = [
    | `T of tag
    | `C of clause
]

(* compilation *)

let rec compile_term : term -> Core.Term.t = function
    | `Variable x -> Core.Term.Make.var x
    | `Integer i ->  Core.Term.Make.int i
    | `Boolean b ->  Core.Term.Make.bool b
    | `Float f ->    Core.Term.Make.float f
    | `Constant s -> Core.Term.Make.const s
    | `Function (f, args) -> args
        |> CCList.map compile_term
        |> Core.Term.Make.apply f

let compile_atom : atom -> Core.Atom.t = function
    | `Atom (name, args) -> args
        |> CCList.map compile_term
        |> Core.Atom.make name

let compile_cost : cost -> Core.Formula.t = function
    | `Sample (var, dist, args, sample_context) ->
        let var = Core.Term.Make.var var in
        let args = CCList.map compile_term args in
        let dist = Core.Term.Make.apply dist args in
        let sample_context = CCList.map compile_term sample_context in
        Core.Formula.Make.draw_from var dist sample_context
    | `True -> Core.Formula.empty

let compile_clause = function
    | `Clause (cost, head, body) ->
        let cost = compile_cost cost in
        let head = compile_atom head in
        let body = CCList.map compile_atom body in
        Core.Clause.make head body
            |> Core.Clause.add_cost cost

let compile lines = lines
    |> CCList.map compile_clause
    |> Core.Program.of_clause_list
