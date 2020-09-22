type t = {
    name : string;
    value : Term.t option;
}

let of_string str = {
    name = str; value = None;
}

let to_json param = `String param.name