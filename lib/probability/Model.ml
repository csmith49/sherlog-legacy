(* interface types *)
type node = Data.Identifier.t
type view = node * node list * Distribution.t
type observation = (node * Core.Term.t) list

(* main type - models are graphs under the hood *)
module Graph = Data.Graph.Make(Data.Identifier)
type t = {
    graph : (Distribution.t, unit) Graph.t;
    observations : observation list;
}

(* access *)
let nodes model = model.graph |> Graph.vertices
let parents node model = model.graph
    |> Graph.incoming node
    |> CCList.map (fun (source, _, _) -> source)
let cpd node model = Graph.label node model.graph |> CCOpt.get_exn (* every node should have a cpd by construction *)

let views model = model
    |> nodes
    |> CCList.map (fun node ->
        (node, parents node model, cpd node model))
let observations model = model.observations

(* construction *)
let empty = {
    graph = Graph.empty;
    observations = [];
}
let add_view (node, parents, distribution) model =
    let graph = model.graph
        |> Graph.add_vertex node distribution (* add the node *)
        |> CCList.fold_right (fun v -> fun g ->
                if Graph.mem v g then g else Graph.add_vertex v Distribution.dummy g
            ) parents (* add the parents, if necessary, with a dummy dist*)
        |> CCList.fold_right (fun p -> fun g -> Graph.add_edge (p, (), node) g) parents (* add edges *)
    in { model with graph = graph; }
let add_observation observation model = { model with observations = observation :: model.observations; }

(* display *)
let view_to_factor_string (x, pa, d) =
    let x' = Data.Identifier.to_string x in
    let pa' = CCList.map Data.Identifier.to_string pa |> CCString.concat ", " in
    let d' = Distribution.to_string d in
    if CCList.is_empty pa then
        "p(" ^ x' ^ ") = " ^ d'
    else
        "p(" ^ x' ^ " | " ^ pa' ^ ") = " ^ d'

let print view model = model.graph
    |> Graph.vertices
    |> CCList.iter (fun vertex ->
        let parents = Graph.incoming vertex model.graph
            |> CCList.map (fun (source, _, _) -> source) in
        let dist = Graph.label vertex model.graph |> CCOpt.get_exn in
        let output = view (vertex, parents, dist) in
            print_endline output
    )

(* and writing out *)
let rec to_json model =
    let observations = model
        |> observations
        |> CCList.map observation_to_json in
    let views = model
        |> views
        |> CCList.map view_to_json in
    `Assoc [
        ("model", `List views);
        ("observations", `List observations);
    ]
and observation_to_json observation =
    let pair_to_json (node, value) = `Assoc [
        ("variable", Data.Identifier.to_json node);
        ("value", Core.Term.to_json value)
    ] in `List (CCList.map pair_to_json observation)
and view_to_json (node, parents, cpd) = `Assoc [
    ("variable", Data.Identifier.to_json node);
    ("dependencies", `List (CCList.map Data.Identifier.to_json parents));
    ("distribution", Distribution.to_json cpd);
]

(* the complicated model-construction stuff goes here *)

(* this module helps contain the compilation pipeline from proof to model *)
module Compile = struct
    (* samples are the nodes we pull from the output of resolution - goal is to convert them to views *)
    type sample = {
        target : Core.Term.t;
        distribution_term : Core.Term.t;
        sampling_context : Core.Term.t list;
    }

    (* our overall structure comes from the form of proofs given by resolution *)
    type 'a dnf = Or of 'a conjunct list
    and 'a conjunct = And of ('a * sample) list

    (* utilities over dnfs *)

    (* extract tags in the same list of list structure *)
    let tags (dnf : 'a dnf) : 'a list list = match dnf with
        | Or conjuncts -> conjuncts
            |> CCList.map (fun conjunct -> match conjunct with
                | And pairs -> CCList.map fst pairs)

    (* map uniformly *)
    let map (f : ('a * sample) -> ('b * sample)) (dnf : 'a dnf) : 'b dnf = match dnf with
        | Or conjuncts -> conjuncts
            |> CCList.map (fun conjunct -> match conjunct with
                | And pairs -> And (CCList.map f pairs))
            |> fun cs -> Or cs

    (* map indexed by conjuncts *)
    let cmap (fs : (('a * sample) -> ('b * sample)) list) (dnf : 'a dnf) : 'b dnf = match dnf with
        | Or conjuncts -> conjuncts
            |> CCList.map2 (fun f -> fun conjunct -> match conjunct with
                | And pairs -> And (CCList.map f pairs)) fs
            |> fun cs -> Or cs

    (* since this transformation is lightweight, we'll avoid ornaments and the like and just give a type per stage *)
    type initial_dnf = unit dnf
    type named_dnf = node dnf
    type annotated_dnf = (node * Distribution.t) dnf
    type connected_dnf = (node * node list * Distribution.t) dnf
    
    (* build initial dnf from "proof" *)
    let rec initialize (proof : Core.Formula.t list) : initial_dnf =
        let conjuncts = CCList.map conjunct_of_formula proof in
        Or conjuncts
    and conjunct_of_formula (formula : Core.Formula.t) : unit conjunct =
        let samples = formula
            |> Core.Formula.conjuncts
            |> CCList.filter_map Core.Formula.Conjunct.decompose_sample
            |> CCList.map (fun (x, d, sc) -> ( (), { target = x; distribution_term = d; sampling_context = sc; } )) in
        And samples

    (* get names for the samples *)

    (* naming is done uniformly and repeatably, if necessary *)
    let name_sample (sample : sample) : node =
        let name = match sample.distribution_term with
            | Core.Term.Function (f, _) -> f
            | _ -> "?" in
        let context = sample.sampling_context |> CCList.map Core.Term.to_string |> CCString.concat ", " in
        name ^ "[" ^ context ^ "]" |> Data.Identifier.of_string 

    (* and lifting is straightforward *)
    let name_samples (dnf : initial_dnf) : named_dnf =
        let f ( (), sample ) = ( name_sample sample, sample ) in map f dnf

    (* annotating samples *)

    (* samples need to be tagged with the cpd, but that requires renaming across conjuncts *)
    (* indexing by conjuncts keeps each execution independent - not sure if necessary, but scared of sharing *)
    (* TODO: explore the above *)

    (* step 1: get per-conjunct sub *)
    let per_conjunct_renaming (dnf : named_dnf) : Core.Substitution.t list =
        let f (name, sample) = match sample.target with
            | Core.Term.Variable target -> (Some (target, Core.Term.Variable name), sample)
            | _ -> (None, sample) in
        dnf |> map f
            |> tags
            |> CCList.map (fun assocs -> assocs
                |> CCList.keep_some
                |> Core.Substitution.of_list
            )

    (* step 2: build distribution in context of sub *)
    let build_distributions (dnf : named_dnf) (subs : Core.Substitution.t list) : annotated_dnf =
        let gs = subs |> CCList.map (fun sub -> fun (name, sample) ->
            let dist = Core.Substitution.apply sample.distribution_term sub
                |> Distribution.of_term
                |> CCOpt.get_exn in
            let tag = (name, dist) in (tag, sample)) in
        cmap gs dnf

    (* all together *)
    let annotate_samples (dnf : named_dnf) : annotated_dnf =
        let subs = per_conjunct_renaming dnf in build_distributions dnf subs

    (* connecting samples *)

    (* simple introspection gives us the parents of each node - we just look for variables in the dists *)
    let connect_samples (dnf : annotated_dnf) : connected_dnf =
        let f ( (name, dist), sample ) =
            let parents = Distribution.variables dist in
            let tag = (name, parents, dist) in
                (tag, sample)
        in map f dnf

    (* final utility *)
    let views (dnf : connected_dnf) : view list = dnf
        |> tags
        |> CCList.flatten

    let observations (dnf : connected_dnf) : observation list =
        let f ( (name, _, _), sample ) = match sample.target with
            | Core.Term.Variable _ -> (None, sample)
            | (_ as term) -> (Some (name, term), sample) in
        dnf |> map f
            |> tags
            |> CCList.map CCList.keep_some
end

(* using the above, we can build a model from a "proof" *)
let of_proof proof =
    let dnf = proof
        |> Compile.initialize
        |> Compile.name_samples
        |> Compile.annotate_samples
        |> Compile.connect_samples in
    let views = Compile.views dnf in
    let observations = Compile.observations dnf in
    empty
        |> CCList.fold_right add_view views
        |> CCList.fold_right add_observation observations