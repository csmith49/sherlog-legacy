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
      let _ = program |> Core.Program.to_string |> print_endline in
      let tree = Core.ResolutionTree.of_query query |> Core.ResolutionTree.resolve program in
      let solutions = Core.ResolutionTree.solutions tree in
      solutions |> CCList.iter (fun _ -> print_endline "solution found")
  | None -> print_endline "Program parsing failed..."