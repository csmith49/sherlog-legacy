module type ELEMENT = CCMap.OrderedType

module type S = sig
    type t
    type element

    val find : element -> t -> element
    val union : element -> element -> t -> t

    val equivalent : element -> element -> t -> bool
end

module Make (E : ELEMENT) : S = struct
    module EMap = CCMap.Make(E)
    type element = E.t

    (* tags maintain ranks for easy path compression *)
    type tag = {
        parent : element option;
        rank : int;
    }

    type t = tag EMap.t

    let tag e m = match EMap.find_opt e m with
        | Some tag -> tag
        | None -> {
            parent = None;
            rank = 0;
        }

    let parent e m = let e_tag = tag e m in e_tag.parent
    let rank e m = let e_tag = tag e m in e_tag.rank

    let update_parent e parent m =
        let e_tag = tag e m in
        let e_tag' = {e_tag with parent = Some parent} in
        EMap.add e e_tag' m
    
    let update_rank e rank m =
        let e_tag = tag e m in
        let e_tag' = {e_tag with rank = rank} in
        EMap.add e e_tag' m

    let rec find e m = match parent e m with
        | Some parent -> find parent m
        | None -> e

    let find_and_compress e m =
        let ancestor = find e m in
        if E.compare e ancestor == 0 then (e, m)
        else
            let m' = update_parent e ancestor m in
            (ancestor, m')

    let equivalent left right m = E.compare (find left m) (find right m) == 0

    let union left right m =
        let left', m = find_and_compress left m in
        let right', m = find_and_compress right m in
        if rank left' > rank right' then
            update_parent right' left' m
        else if rank right' > rank left' then
            update_parent left' right' m
        else let rank' = (rank right' m) + 1 in
            m |> update_parent left' right' |> update_rank right' rank'
end