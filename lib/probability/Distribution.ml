module ArgMap = CCMap.Make(CCString)

module Symbol = struct
    type t = Normal | Bernoulli | Dummy
    
    let of_string = function
        | "normal" | "Normal" | "N" -> Some Normal
        | "bern" | "bernoulli" | "Bern" | "Bernoulli" -> Some Bernoulli
        | _ -> None

    let arguments = function
        | Normal -> [ "mean" ; "sd" ; ]
        | Bernoulli -> [ "success" ; ]
        | Dummy -> []

    let to_string = function
        | Normal -> "Normal"
        | Bernoulli -> "Bernoulli"
        | Dummy -> "Dummy"
    
    let to_json s = `String (to_string s)
end

type t = {
    distribution : Symbol.t;
    arguments : Core.Term.t ArgMap.t;
}

let rec of_term = function
    | Core.Term.Function (f, fs) -> begin try force_of_term f fs with _ -> None end
    | _ -> None
and force_of_term symbol args = match Symbol.of_string symbol with
    | Some symbol -> Some {
        distribution = symbol;
        arguments = args
            |> CCList.map2 CCPair.make (Symbol.arguments symbol)
            |> ArgMap.of_list;
    }
    | None -> None

let to_json dist = `Assoc [
    ("distribution", dist.distribution |> Symbol.to_json);
    ("arguments", `Assoc (dist.arguments |> ArgMap.to_list |> CCList.map (fun (k, v) -> (k, Core.Term.to_json v))));
]

let to_string dist =
    let symbol = Symbol.to_string dist.distribution in
    let args = dist.arguments
        |> ArgMap.to_list
        |> CCList.map (fun (k, v) -> k ^ "=" ^ (Core.Term.to_string v))
        |> CCString.concat ", " in
    symbol ^ "(" ^ args ^ ")"

let dummy = {
    distribution = Symbol.Dummy;
    arguments = ArgMap.empty;
}

let variables dist = dist.arguments
    |> ArgMap.to_list
    |> CCList.map snd
    |> CCList.flat_map Core.Term.variables