type t = {
    subobligation : Predicate.t;
    substitution : Substitution.t;
}
type resolution = t

type strategy = Context.t -> (resolution * Context.t) list

let extend base extension = {
    subobligation = extension.subobligation;
    substitution = Substitution.compose base.substitution extension.substitution;
}


let root = Predicate.make "root" []

let initial = {
    subobligation = root;
    substitution = Substitution.empty;
}

module Result = struct
    type t =
        | Success
        | Failure

    let is_successful = function
        | Success -> true
        | _ -> false
end

module Path = struct
    type t = resolution list

    let terminal_resolution path = CCList.rev path
        |> CCList.head_opt
end

module Tree = struct
    type node =
        | Terminal of Result.t
        | Resolution of resolution * Context.t
    type t = node Data.Tree.tree

    let expand_node strategy = function
        | Terminal _ -> []
        | Resolution (resolution, context) ->
            if Obligation.is_predicate_satisfied (Context.obligation context) then
                [Terminal Result.Success]
            else
                let results = strategy context
                |> CCList.map (fun (r, context') ->
                    let resolution' = extend resolution r in
                        Resolution (resolution', context')
                ) in
                if (CCList.length results) == 0 then [Terminal Result.Failure] else results

    let is_expandable tree =
        if Data.Tree.is_leaf tree then
            match Data.Tree.label tree with
                | Terminal _ -> false
                | Resolution _ -> true
        else false

    let of_query query =
        let node = Resolution (initial, Context.of_query query) in
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
        |> CCList.filter_map Path.terminal_resolution
        |> CCList.map (fun r -> r.substitution)
end