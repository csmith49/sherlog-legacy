module Graph = Data.Graph.Make(Data.Identifier)

type t = (Distribution.t, unit) Graph.t

let empty = Graph.empty

let id_of_sample (_, dist, es) =
    let dist' = Core.Term.to_string dist in
    let es' = es |> CCList.map Core.Term.to_string |> CCString.concat ", " in
    let id = dist' ^ "@[" ^ es' ^ "]" |> Data.Identifier.of_string in id

let of_formula formula =
    let samples = formula
        |> Core.Formula.conjuncts
        |> CCList.filter_map Core.Formula.Conjunct.decompose_sample in
    let vertices = samples
        |> CCList.filter_map (fun (x, dist, es) -> 
            let label = Distribution.of_term dist in match label with
                | Some lbl -> Some (id_of_sample (x, dist, es), lbl)
                | _ -> None
            ) in
    let edges = CCList.product CCPair.make samples samples
        |> CCList.filter_map (fun ((x, xd, xes), (y, yd, yes)) -> match x with
            | Core.Term.Variable x ->
                if CCList.exists (Core.Term.occurs x) (yd :: yes) then
                    Some (id_of_sample (x, xd, xes), (), id_of_sample (y, yd, yes))
                else None
            | _ -> None
            ) in
    empty
        |> CCList.fold_right (fun (k, v) -> fun g -> Graph.add_vertex k v g) vertices
        |> CCList.fold_right (fun e -> fun g -> Graph.add_edge e g) edges

type view = (Data.Identifier.t * Data.Identifier.t list * Distribution.t) -> string

let factor_view (x, pa, d) =
    let x' = Data.Identifier.to_string x in
    let pa' = CCList.map Data.Identifier.to_string pa |> CCString.concat ", " in
    let d' = Distribution.to_string d in
    if CCList.is_empty pa then
        "p(" ^ x' ^ ") = " ^ d'
    else
        "p(" ^ x' ^ " | " ^ pa' ^ ") = " ^ d'

let source (s, _, _) = s

let print view graph = graph
    |> Graph.vertices
    |> CCList.iter (fun vertex ->
        let parents = Graph.incoming vertex graph |> CCList.map source in
        let dist = Graph.label vertex graph |> CCOpt.get_exn in
        let output = view (vertex, parents, dist) in
            print_endline output
    )