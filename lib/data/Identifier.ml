type context = int

type t = {
    name : string;
    index : int;
    context : context;
}

let name id = id.name
let index id = id.index
let context id = id.context

let default_context = 0

let to_context id =
    (id.name |> CCString.hash) + (5 * id.index) + (19 * id.context)

let of_string str = {
    name = str;
    index = 0;
    context = default_context;
}
let to_string id = id.name

let compare left right =
    let name_comparison = CCString.compare left.name right.name in
    if name_comparison != 0 then name_comparison else
    let index_comparison = CCInt.compare left.index right.index in
    if index_comparison != 0 then index_comparison else
    CCInt.compare left.context right.context

let equal left right = (compare left right) == 0

let extend id str = {
    name = str;
    index = 0;
    context = to_context id;
}
let extend_by_index id index = { id with index = index }

let split n id = CCList.range 0 n
    |> CCList.map (fun index -> {
        name = id.name;
        index = index;
        context = id.context;
    })

let to_json id = `String id.name