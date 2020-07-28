open Core.Interface

(* variables *)
let x = var "X"
let y = var "Y"
let z = var "Z"

(* atoms *)
let xerces = atom "xerces"
let brooke = atom "brooke"
let damocles = atom "damocles"

(* predicate symbols *)
let parent l r = Core.Predicate.make "parent_of" [l ; r]

let ancestor l r = Core.Predicate.make "ancestor_of" [l ; r]

(* fact and clause construction *)
let fact pred = {
    Core.Basic.head = pred;
    body = [];
}

(* the program *)
let program = [
    fact (parent xerces brooke);
    fact (parent brooke damocles);
    ancestor x y := [parent x y];
    ancestor x y := [parent x z; ancestor z y];
]

let query = [ancestor xerces damocles]
let results = Core.Basic.resolve program query
let _ = Core.Basic.interpret results
    |> CCList.iter print_endline