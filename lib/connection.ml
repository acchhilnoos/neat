type t = { i_id : int; o_id : int; innov : int; enabled : bool; weight : float }

let init ?(enabled = true) ?(weight = 1.) i_id o_id innov =
  { i_id; o_id; innov; enabled; weight }

let get_i_id c          = c.i_id
let get_o_id c          = c.o_id
let get_innov c         = c.innov
let get_enabled c       = c.enabled
let get_weight c        = c.weight
let toggle c            = { c with enabled = not c.enabled }
let set_weight c weight = { c with weight }
