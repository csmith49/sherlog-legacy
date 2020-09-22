type result = AST.Parse.result
let parse = AST.Parse.parse
let program = AST.Parse.program
let queries = AST.Parse.queries
let evidence = AST.Parse.evidence
let parameters = AST.Parse.parameters

type server = Network.server
type handler = Yojson.Basic.t -> Yojson.Basic.t option
type port = int

let local_server handler port =
    let socket = Network.socket Network.local_address port in
    let server = Network.server handler socket in
        server
let run = Network.run