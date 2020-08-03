(* programs mix EDB and IDB rules *)
type program = Clause.t list

(* nodes track goals and the way they're resolved *)
type rnode =
    | Failure
    | Resolution of Clause.t * Obligation.t
    | Success

type rtree = rnode Data.Tree.tree

let expand_node program = function
    | Resolution (_, ob) when Obligation.is_predicate_satisfied ob -> [Data.Tree.leaf Success]
    | Resolution (_, ob) ->
        let resolve clause = match Clause.apply ob clause with
            | Some ob -> Some (clause, ob)
            | None -> None in
        let resolutions = CCList.filter_map resolve program in
        if CCList.is_empty resolutions then [Data.Tree.leaf Failure]
        else resolutions
            |> CCList.map (fun (c, ob) -> Resolution (c, ob) |> Data.Tree.leaf)
    | _ -> []

let is_expandable tree =
    if Data.Tree.is_leaf tree then 
        match Data.Tree.label tree with
            | Resolution _ -> true
            | _ -> false
    else false

let root_clause = Predicate.make "root" [] |> Clause.fact

let of_query query = Resolution (root_clause, query) |> Data.Tree.leaf

let rec resolve program query = resolve_tree program (of_query query)
and resolve_tree program tree = match Data.Tree.find is_expandable (Data.Tree.zipper tree) with
    | Some zipper ->
        let node = zipper |> Data.Tree.focus |> Data.Tree.label in
        let children = expand_node program node in
        let subtree = Data.Tree.Node (node, children) in
        let tree = zipper
            |> Data.Tree.set_focus subtree
            |> Data.Tree.of_zipper in
        resolve_tree program tree
    | None -> tree

let rec interpret tree = Data.Tree.paths tree
    |> CCList.map interpret_path
and interpret_path path = path
    |> CCList.map (fun node -> match node with
        | Resolution (_, ob) -> Obligation.to_string ob
        | Failure -> "FAIL"
        | Success -> "DONE")
    |> CCString.concat "\n"