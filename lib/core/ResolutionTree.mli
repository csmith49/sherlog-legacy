type t

type resolution
type resolution_path = resolution list

type path
type zipper

val of_query : Predicate.t list -> t
val resolve : Program.t -> t -> t

val solutions : t -> resolution_path list