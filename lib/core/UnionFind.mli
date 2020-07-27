module type ELEMENT = CCMap.OrderedType

module type S = sig
    type t
    type element

    val find : element -> t -> element
    val union : element -> element -> t -> t

    val equivalent : element -> element -> t -> bool
end

module Make : ELEMENT -> S