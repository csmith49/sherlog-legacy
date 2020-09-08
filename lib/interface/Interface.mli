val parse_file : string -> (Core.Query.t * Core.Program.t) option
val parse_string : string -> (Core.Query.t * Core.Program.t) option

module Network : sig
    type handler = Yojson.Basic.t -> Yojson.Basic.t option
    type port = int

    val local_handler_server : port -> handler -> (unit -> unit Lwt.t)
end