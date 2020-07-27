let _ = print_endline "TESTING BUILD"

(* variables *)
let x = Core.Variable "X"
let y = Core.Variable "Y"
let z = Core.Variable "Z"

(* atoms *)
let xerces = Core.Atom "xerces"
let brooke = Core.Atom "brooke"
let damocles = Core.Atom "damocles"

(* predicate symbols *)
let parent_symbol = {
    Core.name = "parent_of";
    arity = 2;
}
let ancestor_symbol = {
    Core.name = "ancestor_of";
    arity = 2;
}

(* predicate construction *)
let parent l r = {
    Core.symbol = parent_symbol;
    args = [l ; r];
}
let ancestor l r = {
    Core.symbol = ancestor_symbol;
    args = [l ; r]
}

(* fact and clause construction *)
let fact pred = {
    Core.head = pred;
    body = [];
}
let clause hd bdy = {
    Core.head = hd;
    body = bdy;
}
let ( <-. ) hd bdy = clause hd bdy

(* the program *)
let program = [
    fact (parent xerces brooke);
    fact (parent brooke damocles);
    ancestor x y <-. [parent x y];
    ancestor x y <-. [parent x z; ancestor z y];
]

let query = [ancestor xerces damocles]
let results = Core.resolve program query
let _ = Core.interpret results
    |> CCList.iter print_endline