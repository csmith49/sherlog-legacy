type t = Atom.t list

let of_atom_list atoms = atoms

let to_json obs = obs
    |> CCList.map Atom.to_json
    |> Data.JSON.Make.list