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
      let strategy = Core.Program.linear_strategy program in
      let tree = Core.Resolution.Tree.of_query query |> Core.Resolution.Tree.resolve strategy in
      let solutions = Core.Resolution.Tree.solutions tree in
      let _ = print_endline ("Num. satisfying branches: " ^ (solutions |> CCList.length |> string_of_int)) in
      solutions |> CCList.iter (fun s -> Core.ProofStep.cost s
          |> Core.Formula.to_string
          |> print_endline
      )
  | None -> print_endline "Program parsing failed..."