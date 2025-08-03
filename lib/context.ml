module Cstate = struct
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
end

module Nstate = struct
  type t = { next_id : int }

  let empty = { next_id = 0 }

  let get t =
    let i = t.next_id in
    (i, { next_id = i + 1 })
end

type c = Cstate.t * Nstate.t * Random.State.t
type 'a t = c -> 'a * c

let return x ct = (x, ct)
let run m ct = m ct

let ( >>= ) m f ct =
  let n, ct' = m ct in
  f n ct'

module Let_syntax = struct
  let ( let* ) = ( >>= )
end

let init () = (Cstate.empty, Nstate.empty, Random.State.make [| 633397 |])

let cget i_id o_id (cs, ns, rs) =
  let innov, cs' = Cstate.get i_id o_id cs in
  (innov, (cs', ns, rs))

let nget (cs, ns, rs) =
  let id, ns' = Nstate.get ns in
  (id, (cs, ns', rs))

let rand_int bd (_, _, rs) = Random.State.int rs bd
let rand_float bd (_, _, rs) = Random.State.float rs bd

let m_rand_int bd (cs, ns, rs) =
  let i = Random.State.int rs bd in
  (i, (cs, ns, rs))

let m_rand_float bd (cs, ns, rs) =
  let i = Random.State.float rs bd in
  (i, (cs, ns, rs))
