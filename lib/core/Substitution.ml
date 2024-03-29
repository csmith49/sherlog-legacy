module IdMap = CCMap.Make(Data.Identifier)

type t = Term.t IdMap.t

let empty = IdMap.empty

let add = IdMap.add
let add_all ps m = IdMap.add_list m ps
let of_list = IdMap.of_list

let compose left right = of_list ((IdMap.to_list left) @ (IdMap.to_list right))

let to_string sub = sub
    |> IdMap.to_list
    |> CCList.map (fun (k, v) -> 
            (Data.Identifier.to_string k) ^ "/" ^ (Term.to_string v)
        )
    |> CCString.concat ", "
    |> (fun s -> "[" ^ s ^ "]")

let rec apply term sub = match term with
    | Term.Variable x -> begin match IdMap.find_opt x sub with
        | Some term' -> apply term' sub
        | None -> term
    end
    | Term.Function (f, args) ->
        let args' = CCList.map (fun t -> apply t sub) args in Term.Make.apply f args'
    | _ -> term

let lookup_identifier id sub =
    let term = Term.Variable id in
    apply term sub