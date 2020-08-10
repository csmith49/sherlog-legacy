module State = struct
    type t = {
        obligation : Obligation.t;
    }

    let obligation state = state.obligation

    let extend_obligation preds state = {
        obligation = Obligation.add_all preds state.obligation;
    }

    let substitute state sub = {
        obligation = Obligation.substitute state.obligation sub;
    }

    let set_obligation obligation _ = {
        obligation = obligation;
    }

    let initial = {
        obligation = [] |> Query.of_predicate_list |> Obligation.of_query;
    }

    let of_query query = initial |> set_obligation (Obligation.of_query query)
    
    let discharge_predicate state = match Obligation.discharge_predicate state.obligation with
        | Some (predicate, obligation) -> Some (predicate, state |> set_obligation obligation)
        | _ -> None
end

module Step = struct
    type t = {
        cost : Formula.t;
        subobligation : Predicate.t;
        substitution : Substitution.t;
    }
    type step = t

    let make pred sub cost = {
        cost = cost;
        subobligation = pred;
        substitution = sub;
    }
    
    let initial = make (Predicate.make "root" []) Substitution.empty Formula.empty

    module Derivation = struct
        type t = step list
    
        let of_step_list ls = ls
        let to_step_list ls = ls
    
        let summary der =
            let cost, sub = der
                |> to_step_list
                |> CCList.map (fun step -> 
                    let sub = step.substitution in
                    let cost = Formula.substitute step.cost sub in
                        (cost, sub)
                ) |> CCList.fold_left (fun (c, s) -> fun (c', s') ->
                    let cost = Formula.conjoin c c' in
                    let sub = Substitution.compose s s' in
                        (cost, sub)
                ) (Formula.empty, Substitution.empty) in
            (Formula.substitute cost sub |> Formula.simplify, sub)
    
        let cost der = der |> summary |> fst
    end
end

type strategy = State.t -> (Step.t * State.t) list

module Tree = struct
    type result =
        | Success
        | Failure

    type node =
        | Terminal of result
        | Resolution of Step.t * State.t
    type t = node Data.Tree.tree

    let expand_node strategy = function
        | Terminal _ -> []
        | Resolution (_, state) ->
            if Obligation.is_predicate_satisfied (State.obligation state) then
                [Terminal Success]
            else
                let results = strategy state
                    |> CCList.map (fun (step, state) -> Resolution (step, state)
                ) in
                if (CCList.length results) == 0 then [Terminal Failure] else results

    let is_expandable tree =
        if Data.Tree.is_leaf tree then
            match Data.Tree.label tree with
                | Terminal _ -> false
                | Resolution _ -> true
            else false

    let of_query query =
        let node = Resolution (Step.initial, State.of_query query) in
            Data.Tree.leaf node

    let rec resolve_zipper strategy zipper = match Data.Tree.find is_expandable zipper with
        | Some zipper ->
            let node = zipper |> Data.Tree.focus |> Data.Tree.label in
            let children = expand_node strategy node
                |> CCList.map Data.Tree.leaf in
            let tree' = Data.Tree.Node (node, children) in
            let zipper' = zipper |> Data.Tree.set_focus tree' in
                resolve_zipper strategy zipper'
        | None -> zipper
    
    let resolve strategy tree = tree
        |> Data.Tree.zipper
        |> resolve_zipper strategy
        |> Data.Tree.of_zipper

    let successful_branch branch = match branch |> CCList.rev |> CCList.head_opt with
        | Some (Terminal Success) -> true
        | _ -> false
    
    let path_of_branch branch = branch
        |> CCList.filter_map (fun node -> match node with
            | Resolution (resolution, _) -> Some resolution
            | _ -> None
        )
    
    let solutions tree = tree
        |> Data.Tree.paths
        |> CCList.filter successful_branch
        |> CCList.map path_of_branch
        |> CCList.map Step.Derivation.of_step_list
end