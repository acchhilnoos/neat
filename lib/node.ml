type kind = Input | Bias | Hidden | Output
type t = { id : int; kind : kind; value : float }

let init ?(value = 0.) id kind = { id; kind; value }
let get_id n = n.id
let get_kind n = n.kind
let get_value n = n.value
