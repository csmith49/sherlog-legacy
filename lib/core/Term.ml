type t =
    | Variable of Data.Identifier.t
    | Integer of int
    | Float of float
    | Boolean of bool
    | Constant of string
    | Function of string * t list

let rec to_string = function
    | Variable x -> Data.Identifier.to_string x
    | Integer i -> string_of_int i
    | Float f -> string_of_float f
    | Boolean b -> string_of_bool b
    | Constant a -> a
    | Function (f, args) ->
        let args' = CCList.map to_string args in
        f ^ "(" ^ (CCString.concat ", " args') ^ ")"

let rec equal left right = match left, right with
    | Variable x, Variable y -> Data.Identifier.equal x y
    | Integer x, Integer y -> CCInt.equal x y
    | Float x, Float y -> CCFloat.equal x y
    | Boolean x, Boolean y -> CCBool.equal x y
    | Constant x, Constant y -> CCString.equal x y
    | Function (f, fs), Function (g, gs) ->
        (CCString.equal f g) && (CCList.for_all2 equal fs gs) 
    | _ -> false

let rec occurs id term = match term with
    | Variable x -> Data.Identifier.equal id x
    | Function (_, args) -> CCList.exists (occurs id) args
    | _ -> false

let rec is_ground = function
    | Variable _ -> false
    | (Integer _ | Float _ | Boolean _ | Constant _) -> true
    | Function (_, args) -> CCList.for_all is_ground args

let rec variables = function
    | Variable x -> [x]
    | Function (_, args) -> CCList.flat_map variables args
    | _ -> []

let rec to_json = function
    | Variable x -> `Assoc [
        ("type", `String "variable");
        ("value", Data.Identifier.to_json x);
    ]
    | Integer i -> `Assoc [
        ("type", `String "integer");
        ("value", `Int i);
    ]
    | Float f -> `Assoc [
        ("type", `String "float");
        ("value", `Float f);
    ]
    | Boolean b -> `Assoc [
        ("type", `String "boolean");
        ("value", `Bool b);
    ]
    | Constant c -> `Assoc [
        ("type", `String "constant");
        ("value", `String c);
    ]
    | Function (f, fs) -> `Assoc [
        ("type", `String "function");
        ("value", `String f);
        ("arguments", `List (CCList.map to_json fs));
    ]

(* TODO - optimize *)
let rec hash = function
    | Variable x -> x 
        |> Data.Identifier.to_string
        |> CCHash.string
    | Integer i -> CCHash.int i
    | Float f -> CCFloat.hash f
    | Boolean b -> CCHash.bool b
    | Constant c -> CCHash.string c
    | Function (f, fs) ->
        CCHash.combine2 (CCHash.string f) (CCHash.list hash fs)

module Make = struct
    let var x = Variable (x |> Data.Identifier.of_string)
    let int i = Integer i
    let float f = Float f
    let bool b = Boolean b
    let const s = Constant s
    let apply f args = Function (f, args)
end