type t = { i_id : int; o_id : int; innov : int; enabled : bool; weight : float }

let init ?(enabled = true) ?(weight = 1.) i_id o_id innov =
  { i_id; o_id; innov; enabled; weight }

let get_i_id c = c.i_id
let get_o_id c = c.o_id
let get_innov c = c.innov
let get_enabled c = c.enabled
let get_weight c = c.weight
let enable c = { c with enabled = true }
let disable c = { c with enabled = false }
let set_weight c weight = { c with weight }

let pp fmt c =
  Format.fprintf fmt "{%3d %3d: %3d %5b %6.3f}" c.i_id c.o_id
    c.innov c.enabled c.weight
