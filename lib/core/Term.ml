type t =
    | Variable of Data.Identifier.t
    | Integer of int
    | Float of float
    | Boolean of bool
    | Atom of string
    | Function of string * t list

let rec to_string = function
    | Variable x -> Data.Identifier.to_string x
    | Integer i -> string_of_int i
    | Float f -> string_of_float f
    | Boolean b -> string_of_bool b
    | Atom a -> a
    | Function (f, args) ->
        let args' = CCList.map to_string args in
        f ^ "(" ^ (CCString.concat ", " args') ^ ")"

let rec equal left right = match left, right with
    | Variable x, Variable y -> Data.Identifier.equal x y
    | Integer x, Integer y -> CCInt.equal x y
    | Float x, Float y -> CCFloat.equal x y
    | Boolean x, Boolean y -> CCBool.equal x y
    | Atom x, Atom y -> CCString.equal x y
    | Function (f, fs), Function (g, gs) ->
        (CCString.equal f g) && (CCList.for_all2 equal fs gs) 
    | _ -> false

let rec occurs id term = match term with
    | Variable x -> Data.Identifier.equal id x
    | Function (_, args) -> CCList.exists (occurs id) args
    | _ -> false

module Make = struct
    let var x = Variable (x |> Data.Identifier.of_string)
    let int i = Integer i
    let float f = Float f
    let bool b = Boolean b
    let atom s = Atom s
    let apply f args = Function (f, args)
end