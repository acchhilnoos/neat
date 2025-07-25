type kind = Input | Bias | Hidden of int | Output
type t = { id : int; kind : kind; value : float }

let init ?(value = 0.) id kind = { id; kind; value }
let get_id n = n.id
let get_kind n = n.kind
let get_layer n = match n.kind with Hidden x -> x | Output -> -1 | _ -> 0
let get_value n = n.value

let inc_layer n =
  match n.kind with Hidden i -> { n with kind = Hidden (i + 1) } | _ -> n
