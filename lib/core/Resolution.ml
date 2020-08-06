type strategy = ProofState.t -> (ProofStep.t * ProofState.t) list

module Result = struct
    type t =
        | Success
        | Failure

    let is_successful = function
        | Success -> true
        | _ -> false
end

module Tree = struct
    type node =
        | Terminal of Result.t
        | Resolution of ProofStep.t * ProofState.t
    type t = node Data.Tree.tree

    let expand_node strategy = function
        | Terminal _ -> []
        | Resolution (_, state) ->
            if Obligation.is_predicate_satisfied (ProofState.obligation state) then
                [Terminal Result.Success]
            else
                let results = strategy state
                |> CCList.map (fun (step, state) -> Resolution (step, state)
                ) in
                if (CCList.length results) == 0 then [Terminal Result.Failure] else results

    let is_expandable tree =
        if Data.Tree.is_leaf tree then
            match Data.Tree.label tree with
                | Terminal _ -> false
                | Resolution _ -> true
        else false

    let of_query query =
        let node = Resolution (ProofStep.initial, ProofState.of_query query) in
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
        | Some (Terminal Result.Success) -> true
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
        |> CCList.map ProofStep.Derivation.of_step_list
end