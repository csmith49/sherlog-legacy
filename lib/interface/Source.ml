type line =
  | EDB of Core.Clause.t
  | IDB of Core.Predicate.t list

let rec split_lines = function
  | [] -> ([], [])
  | EDB clause :: rest -> begin match split_lines rest with
    | (i, e) -> (i, clause :: e)
  end
  | IDB query :: rest -> begin match split_lines rest with
    | (i, e) -> (query @ i, e)
  end

let process_lines lines =
  let query, clauses = split_lines lines in
  (Core.Query.of_predicate_list query, Core.Program.of_clause_list clauses)