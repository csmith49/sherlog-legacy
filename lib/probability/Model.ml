module IdMap = CCMap.Make(Data.Identifier)

type t = {
    nodes : Data.Identifier.t list;
    parents : (Data.Identifier.t list) IdMap.t;
    cpds : Distribution.t IdMap.t;
}

let empty = {
    nodes = [];
    parents = IdMap.empty;
    cpds = IdMap.empty;
}

let of_list vs =
    let add_view model (node, parents, dist) = {
            nodes = node :: model.nodes;
            parents = IdMap.add node parents model.parents;
            cpds = IdMap.add node dist model.cpds;
    } in CCList.fold_left add_view empty vs

let to_list model = model.nodes
    |> CCList.filter_map (fun id -> match IdMap.find_opt id model.parents, IdMap.find_opt id model.cpds with
        | Some parents, Some dist -> Some (id, parents, dist)
        | _ -> None
    )

let to_json model =
    let views = model
        |> to_list
        |> CCList.map (fun (node, parents, dist) -> `Assoc [
            ("node", Data.Identifier.to_json node);
            ("parents", `List (CCList.map Data.Identifier.to_json parents));
            ("distribution", Distribution.to_json dist)
        ]) in
    `List views


let of_formula _ = Some empty