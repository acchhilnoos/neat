type kind = Input | Bias | Hidden of int | Output
type t = { id : int; kind : kind; value : float }

let init ?(value = 0.) id kind = { id; kind; value }
let get_id n = n.id
let get_kind n = n.kind
let get_layer n = match n.kind with Hidden x -> x | Output -> -1 | _ -> 0
let get_value n = n.value

let inc_layer n =
  match n.kind with Hidden i -> { n with kind = Hidden (i + 1) } | _ -> n

let set_value n value = { n with value }

let pp_kind fmt = function
  | Input -> Format.fprintf fmt "Input     "
  | Bias -> Format.fprintf fmt "Bias      "
  | Hidden l -> Format.fprintf fmt "Hidden(%2d)" l
  | Output -> Format.fprintf fmt "Output    "

let pp fmt nd =
  Format.fprintf fmt "{%3d %a: %6.3f}" nd.id pp_kind nd.kind
    nd.value
