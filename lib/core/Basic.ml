(* clauses maintain heads and bodies - no separation of rules with facts *)
type clause = {
    head : Predicate.t;
    body : Predicate.t list;
}

let string_of_clause clause =
    let head = Predicate.to_string clause.head in
    let body = clause.body
        |> CCList.map Predicate.to_string
        |> CCString.concat ", " in
    if CCList.is_empty clause.body then head
    else
        head ^ " :- " ^ body


let resolve_clause pred clause = match Predicate.unify pred clause.head with
    | Some sub ->
        let obligation = clause.body
            |> CCList.map (fun p -> Predicate.substitute p sub) in
        Some obligation
    | None -> None

(* programs mix EDB and IDB rules *)
type program = clause list

(* we try to eliminate goals as much as possible *)
type goal = Predicate.t list
let satisfied goal = CCList.is_empty goal
let peel = CCList.head_opt

let resolve_goal program goal = match goal with
    | [] -> []
    | pred :: rest -> program
        |> CCList.filter_map (fun c -> resolve_clause pred c)
        |> CCList.map (fun ob -> rest @ ob)

let generate_resolution goal clause = match goal with
    | predicate :: rest -> begin match Predicate.unify predicate clause.head with
        | Some sub -> 
            let obligation = rest @ clause.body |> CCList.map (fun p -> Predicate.substitute p sub) in
            Some obligation
        | None -> None
    end
    | _ -> None 

(* nodes track goals and the way they're resolved *)
type rnode =
    | Failure
    | Resolution of clause * goal
    | Success

type rtree = rnode Data.Tree.tree

let expand_node program = function
    | Resolution (_, []) -> [Data.Tree.leaf Success]
    | Resolution (_, goal) ->
        let resolve clause = match generate_resolution goal clause with
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

let root_clause = {
    head = Predicate.make "root" []; 
    body = []; }

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
        | Resolution (_, goal) -> CCString.concat ", " (CCList.map Predicate.to_string goal)
        | Failure -> "FAIL"
        | Success -> "DONE")
    |> CCString.concat "\n"