type t =
    | Variable of Data.Identifier.t
    | Integer of int
    | Boolean of bool
    | Atom of string

let to_string = function
    | Variable x -> Data.Identifier.to_string x
    | Integer i -> string_of_int i
    | Boolean b -> string_of_bool b
    | Atom a -> a

let equal left right = match left, right with
    | Variable x, Variable y -> Data.Identifier.equal x y
    | Integer x, Integer y -> CCInt.equal x y
    | Boolean x, Boolean y -> CCBool.equal x y
    | Atom x, Atom y -> CCString.equal x y
    | _ -> false

let occurs id term = match term with
    | Variable x -> Data.Identifier.equal id x
    | _ -> false