type line =
    | EDB of Core.Clause.t
    | IDB of Core.Atom.t list

val process_lines : line list -> (Core.Query.t * Core.Program.t)