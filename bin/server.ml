(* reference for argument parsing *)
let port = ref 7999 (* default port, changeable via args *)
let verbose = ref false (* whether or not we print out results when handling messages *)

(* argument parsing *)
let spec_list = [
    ("--port", Arg.Set_int port, "Port to host local server on");
    ("--verbose", Arg.Set verbose, "Enable status updates to StdOut");
]
let usage_msg = "Server for GDL"
let _ = Arg.parse spec_list print_endline usage_msg

let verbose_print str = if !verbose then print_endline str else ()

(* message handling *)

(* for evaluation purposes, we keep some state *)
let program = ref (Core.Program.of_clause_list [])

(* decompose json into commands/messages/arguments *)
let decompose json =
    let command = Data.JSON.Parse.(find string   "command"   json) in
    let message = Data.JSON.Parse.(find identity "message"   json) in
    let args    = Data.JSON.Parse.(find identity "arguments" json) in
    match command, message, args with
        | Some command, Some message, Some args -> Some (command, message, args)
        | Some command, Some message, None      -> Some (command, message, `Null) (* args are optional *)
        | _                                     -> None

let handler json = match decompose json with
    (* echo the input - for test purposes *)
    | Some ("echo", message, _) -> Some message
    (* parse the string as if it were the contents of a file *)
    | Some ("parse", `String message, _) -> 
        let parse_result = Interface.parse message in
        let program = Interface.program parse_result in
        let parameters = Interface.parameters parse_result in
        let evidence = Interface.evidence parse_result in
        Some (`Assoc [
            ("program", Core.Program.to_json program);
            ("evidence", `List (CCList.map Core.Evidence.to_json evidence));
            ("parameters", `List (CCList.map Core.Parameter.to_json parameters));
        ])
    (* register a provided program as a piece of global state *)
    | Some ("register", prog, _) -> begin match Core.Program.of_json prog with
        | Some prog ->
            let _ = program := prog in
            Some (`Assoc [("result", `String "success")])
        | _ -> None end
    (* evaluate the provided query on the stored program *)
    | Some ("query", query, _) -> begin match Core.Query.of_json query with
        | Some query ->
            let strategy = Core.Program.linear_strategy !program in
            let tree = Core.Proof.Tree.of_query query
                |> Core.Proof.Tree.resolve strategy in
            let solutions = tree
                |> Core.Proof.Tree.solutions
                |> CCList.map Core.Proof.Step.Derivation.summary in
            let proof = CCList.map fst solutions in
            let model = Probability.Model.of_proof proof in
                Some (Probability.Model.to_json model)
        | _ -> None end
    | _ -> None

(* main *)
let _ =
    let server = Interface.local_server handler !port in
    Interface.run server