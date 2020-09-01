(* IO *)
let filepath = ref ""
let verbose = ref false

let spec_list = [
  ("--input", Arg.Set_string filepath, "Filepath for logic program file");
]

let usage_msg = "Logic Program Interpreter"
let _ = Arg.parse spec_list print_endline usage_msg

let _ = match Interface.parse_file !filepath with
  | Some (query, program) ->
        let _ = [
            "---< PROGRAM >---";
            (Core.Program.to_string program);
            "---<  QUERY  >---";
            (Core.Query.to_string query);
         ] |> CCString.concat "\n" |> print_endline in
        let strategy = Core.Program.linear_strategy program in
        let tree = Core.Proof.Tree.of_query query |> Core.Proof.Tree.resolve strategy in
        let solutions = tree 
            |> Core.Proof.Tree.solutions 
            |> CCList.map Core.Proof.Step.Derivation.summary in
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
        ) |> CCString.concat "\n" |> print_endline in
        let proof = CCList.map fst solutions in
        let model = Probability.Model.of_proof proof in
        let _ = print_endline "---<  MODEL  >---" in
        let _ = Probability.Model.print Probability.Model.view_to_factor_string model in
        let _ = print_endline "---<   OBS   >---" in
        let _ = model
            |> Probability.Model.observations
            |> CCList.map Core.Substitution.of_list
            |> CCList.map Core.Substitution.to_string
            |> CCList.iter print_endline
        in ()
  | None -> print_endline "Program parsing failed..."