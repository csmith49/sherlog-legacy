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

let rec to_json = function
    | Variable x -> `Assoc [
        mk_type "variable";
        x |> Data.Identifier.to_string |> Data.JSON.Make.string |> mk_value;
    ]
    | Integer i -> `Assoc [
        mk_type "integer";
        i |> Data.JSON.Make.int |> mk_value;
    ]
    | Float f -> `Assoc [
        mk_type "float";
        f |> Data.JSON.Make.float |> mk_value;
    ]
    | Boolean b -> `Assoc [
        mk_type "bool";
        b |> Data.JSON.Make.bool |> mk_value;
    ]
    | Constant c -> `Assoc [
        mk_type "constant";
        c |> Data.JSON.Make.string |> mk_value;
    ]
    | Function (f, fs) -> `Assoc [
        mk_type "function";
        ("function", `String f);
        ("arguments", fs |> CCList.map to_json |> Data.JSON.Make.list);
    ]
and mk_type typ = ("type", `String typ)
and mk_value v = ("value", v)

let rec of_json = function
    | (`Assoc _) as json ->
        (* get the type *)
        begin match Data.JSON.Parse.(find string "type" json) with
            | Some "integer" -> json
                |> Data.JSON.Parse.(find int "value")
                |> CCOpt.map Make.int
            | Some "float" -> json
                |> Data.JSON.Parse.(find float "value")
                |> CCOpt.map Make.float
            | Some "boolean" -> json
                |> Data.JSON.Parse.(find bool "value")
                |> CCOpt.map Make.bool
            | Some "constant" -> json
                |> Data.JSON.Parse.(find string "value")
                |> CCOpt.map Make.const
            | Some "variable" -> json
                |> Data.JSON.Parse.(find string "value")
                |> CCOpt.map Make.var
            | Some "function" ->
                let f = Data.JSON.Parse.(find string "function" json) in
                let args = Data.JSON.Parse.(find (list of_json) "arguments" json) in
                CCOpt.map2 Make.apply f args
            | _ -> None
        end
    | _ -> None