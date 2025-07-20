type kind = Input | Bias | Hidden | Output
type t = { id : int; kind : kind; value : float }

let id_counter = ref 0
let reset_id () = id_counter := 0

let next_id () =
  let id = !id_counter in
  id_counter := id + 1;
  id

let init kind =
  let id = next_id () in
  let value = 0. in
  { id; kind; value }

let get_id n = n.id
let get_kind n = n.kind
let get_value n = n.value
