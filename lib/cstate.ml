module History_Map = Map.Make (struct
  type t = int * int

  let compare (a, b) (c, d) =
    match Int.compare a c with 0 -> Int.compare b d | x -> x
end)

type t = { map : int History_Map.t; next_innov : int }

let empty = { map = History_Map.empty; next_innov = 0 }

let get i_id o_id t =
  match History_Map.find_opt (i_id, o_id) t.map with
  | Some i -> (i, t)
  | None ->
      let i = t.next_innov in
      ( i,
        {
          map = History_Map.add (i_id, o_id) i t.map;
          next_innov = t.next_innov + 1;
        } )
