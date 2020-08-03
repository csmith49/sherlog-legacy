module T = Data.Tree

type result =
    | Success
    | Failure

type resolution = R
let empty_resolution = R
type resolution_path = resolution list

type node =
    | Terminal of result
    | Resolution of resolution * Obligation.t

type t = node T.tree
type path = node T.path
type zipper = node T.zipper

let expand_node program = function
    | Terminal _ -> []
    | Resolution (resolution, obligation) -> 
        if Obligation.is_predicate_satisfied obligation then
            [Terminal Success]
        else match Program.resolve obligation program with
            | [] -> [Terminal Failure]
            | obligations -> obligations |> CCList.map (fun ob -> Resolution (resolution, ob))

let is_expandable tree =
    if T.is_leaf tree then
        match T.label tree with
            | Terminal _ -> false
            | Resolution _ -> true
    else false

let of_query query =
    let obligation = Obligation.of_query query in
    let resolution = empty_resolution in
    let node = Resolution (resolution, obligation) in
        T.Node (node, [])

let rec resolve_zipper program zipper = match T.find is_expandable zipper with
    | Some zipper ->
        let node = zipper |> T.focus |> T.label in
        let children = expand_node program node
            |> CCList.map T.leaf in
        let subtree = T.Node (node, children) in
        zipper
            |> T.set_focus subtree
            |> resolve_zipper program
    | None -> zipper

let resolve program tree = tree
    |> T.zipper
    |> resolve_zipper program
    |> T.of_zipper

let get_resolution = function
    | Terminal _ -> None
    | Resolution (resolution, _) -> Some resolution

let is_successful_path path = match path |> CCList.rev |> CCList.head_opt with
    | Some (Terminal Success) -> true
    | _ -> false

let solutions tree = tree
    |> T.paths
    |> CCList.filter is_successful_path
    |> CCList.map (CCList.filter_map get_resolution)