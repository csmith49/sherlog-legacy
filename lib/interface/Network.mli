(* message handlers consume json, update state, and produce json *)
type handler = Yojson.Basic.t -> Yojson.Basic.t option

(* type aliases based around Lwt's wrappers *)
type port = int
type address = Lwt_unix.inet_addr
type socket

(* building sockets and serving handlers *)
val local_address : address
val socket : address -> port -> socket
val server : handler -> socket -> (unit -> unit Lwt.t)