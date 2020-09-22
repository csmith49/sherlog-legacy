type result

val parse : string -> result

val program : result -> Core.Program.t
val queries : result -> Core.Query.t list
val evidence : result -> Core.Evidence.t list
val parameters : result -> Core.Parameter.t list

type server
type handler = Yojson.Basic.t -> Yojson.Basic.t option
type port = int

val local_server : handler -> port -> server
val run : server -> unit