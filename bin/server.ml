let port = 7999


let test_handler json = match json with
    | `String s -> Some (`String ("got: " ^ s))
    | _ -> None

(* main *)
let _ =
    let server = Interface.Network.local_handler_server port test_handler in
    Lwt_main.run @@ server ()