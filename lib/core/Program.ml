type t = Clause.t list
type program = t

let of_clause_list p = p
let to_clause_list p = p

let to_string program = program
    |> to_clause_list
    |> CCList.map Clause.to_string
    |> CCList.map (fun cs -> cs ^ ".")
    |> CCString.concat "\n"

let resolve obligation program = program |> CCList.filter_map (Clause.apply obligation)

let try_cache state = match Proof.State.discharge_atom state with
    | Some (atom, state) ->
        if CCList.exists (fun p -> Atom.equal p atom) (Proof.State.cache state) then
            Some state
        else None
    | None -> None

let linear_strategy program state = match try_cache state with
    | Some state -> [ (Proof.Step.initial, state) ]
    | None -> program
        |> to_clause_list
        |> CCList.flat_map (Clause.resolve state)

module DependencyGraph = struct
    (* edges are labeled as special or not based on use of abduced constraints *)
    type label = Special | Normal

    module Graph = Data.Graph.Make(Atom.Position)
    module IdSet = CCSet.Make(Data.Identifier)

    type t = (unit, label) Graph.t

    let empty = Graph.empty

    let merge left right = right
        |> CCList.fold_right (fun v -> fun g -> Graph.add_vertex v () g) (Graph.vertices left)
        |> CCList.fold_right (fun e -> fun g -> Graph.add_edge e g) (Graph.edges left)

    let of_clause clause =
        let e_vars = clause |> Clause.existential_variables |> IdSet.of_list in
        let c_vars = clause |> Clause.cost_variables |> IdSet.of_list in
        let h_pos = clause |> Clause.head |> Atom.positions in
        let b_pos = clause |> Clause.body |> CCList.flat_map Atom.positions in
        let vertices = h_pos @ b_pos |> CCList.map snd in
        let edge_candidates = CCList.product CCPair.make b_pos h_pos in
        let edges = edge_candidates |> CCList.filter_map (fun ((sv, sp), (dv, dp)) ->
                if Data.Identifier.equal sv dv then
                    Some (sp, Normal, dp)
                else if (IdSet.mem dv e_vars) && (IdSet.mem sv c_vars) then
                    Some (sp, Special, dp)
                else None
            ) in
        Graph.empty
            |> CCList.fold_right (fun v -> fun g -> Graph.add_vertex v () g) vertices
            |> CCList.fold_right (fun e -> fun g -> Graph.add_edge e g) edges

    let of_program program = program
        |> CCList.map of_clause
        |> CCList.fold_left merge empty

    (* let is_special_edge = function
        | (_, Special, _) -> true
        | _ -> false
    let is_special_vertex vertex graph = graph
        |> Graph.incoming vertex
        |> CCList.exists is_special_edge *)
end

(* JSON *)
let to_json prog = prog
    |> to_clause_list
    |> CCList.map Clause.to_json
    |> Data.JSON.Make.list

let of_json json = Data.JSON.Parse.list Clause.of_json json |> of_clause_list