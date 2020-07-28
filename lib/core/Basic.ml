(* predicate symbols track arity, but there's not much need for that except overloading *)
type predicate_symbol = {
    name : string;
    arity : int;
    (* if we wanted sorts, they would go here *)
}

(* again, should be in a module *)
let predicate_symbol_eq left right = CCString.equal left.name right.name
    && CCInt.equal left.arity right.arity

(* zero checking to ensure predicates are well-formed wrt their symbol's arity *)
type predicate = {
    symbol : predicate_symbol;
    args : Term.t list;
}

let string_of_predicate pred =
    let symbol = pred.symbol.name in
    let args = pred.args
        |> CCList.map Term.to_string
        |> CCString.concat ", " in
    symbol ^ "(" ^ args ^ ")"

let apply sub pred = {
    pred with args = pred.args |> CCList.map (fun t -> Substitution.apply t sub)
}

(* predicate unification - the aux function is actually relatively standard, if messy *)
let unify left right = 
    if predicate_symbol_eq left.symbol right.symbol then
        Unification.solve (CCList.map2 (fun x -> fun y -> (x, y)) left.args right.args)
    else None

(* clauses maintain heads and bodies - no separation of rules with facts *)
type clause = {
    head : predicate;
    body : predicate list;
}

let string_of_clause clause =
    let head = string_of_predicate clause.head in
    let body = clause.body
        |> CCList.map string_of_predicate
        |> CCString.concat ", " in
    if CCList.is_empty clause.body then head
    else
        head ^ " :- " ^ body


let resolve_clause pred clause = match unify pred clause.head with
    | Some sub ->
        let obligation = clause.body
            |> CCList.map (apply sub) in
        Some obligation
    | None -> None

(* programs mix EDB and IDB rules *)
type program = clause list

(* we try to eliminate goals as much as possible *)
type goal = predicate list
let satisfied goal = CCList.is_empty goal
let peel = CCList.head_opt

let resolve_goal program goal = match goal with
    | [] -> []
    | pred :: rest -> program
        |> CCList.filter_map (fun c -> resolve_clause pred c)
        |> CCList.map (fun ob -> rest @ ob)

let generate_resolution goal clause = match goal with
    | predicate :: rest -> begin match unify predicate clause.head with
        | Some sub -> 
            let obligation = rest @ clause.body |> CCList.map (apply sub) in
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
    head = {symbol = {name = "root"; arity = 0; }; args = []; }; body = []; }

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
        | Resolution (_, goal) -> CCString.concat ", " (CCList.map string_of_predicate goal)
        | Failure -> "FAIL"
        | Success -> "DONE")
    |> CCString.concat "\n"