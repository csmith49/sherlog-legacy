module T = Data.Tree

type result =
    | Success
    | Failure

type resolution = R

type node =
    | Terminal of result
    | Resolution of resolution * Obligation.t

type t = node T.tree
type path = node T.path
type zipper = node T.zipper

let root = Clause.fact (Predicate.make "root" [])