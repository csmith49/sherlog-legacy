(* IO *)
let filepath =              ref ""
let model_output_filepath = ref ""
let verbose =               ref false

let spec_list = [
    ("--input", Arg.Set_string filepath,              "Filepath for logic program file");
    ("--model", Arg.Set_string model_output_filepath, "Filepath for model output");
]

let usage_msg = "Logic Program Interpreter"
let _ = Arg.parse spec_list print_endline usage_msg

let contents =
    let channel = open_in !filepath in
    let length = in_channel_length channel in
        really_input_string channel length

let parse_results = Interface.parse contents

let query = parse_results
    |> Interface.queries
    |> CCList.map Core.Query.to_atom_list
    |> CCList.flatten
    |> Core.Query.of_atom_list

let program = parse_results
    |> Interface.program

let _ = [
    "---< PROGRAM >---";
    (Core.Program.to_string program);
    "---<  QUERY  >---";
    (Core.Query.to_string query);
] |> CCString.concat "\n" |> print_endline

let strategy = Core.Program.linear_strategy program
let tree = Core.Proof.Tree.of_query query
    |> Core.Proof.Tree.resolve strategy

let solutions = tree
    |> Core.Proof.Tree.solutions
    |> CCList.map Core.Proof.Step.Derivation.summary

let _ = [
    "---< RESULTS >---";
] @ (
    solutions
        |> CCList.map (fun (c, s) ->
            let cost = Core.Formula.to_string c in
            let sub = s
                |> Core.Query.summarize_substitution query
                |> Core.Substitution.to_string in
            cost ^ " : " ^ sub
        )
) |> CCString.concat "\n" |> print_endline

let proof = CCList.map fst solutions

let model = Probability.Model.of_proof proof

let _ = print_endline "---<  MODEL  >---"
let _ = Probability.Model.print Probability.Model.view_to_factor_string model
let _ = print_endline "---<   OBS   >---" 
let _ = model
    |> Probability.Model.observations
    |> CCList.map Core.Substitution.of_list
    |> CCList.map Core.Substitution.to_string
    |> CCList.iter print_endline

(* printing the model out to file *)
let _ = if not (CCString.is_empty !model_output_filepath) then
    Yojson.Basic.to_file !model_output_filepath (Probability.Model.to_json model)