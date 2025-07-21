type kind = Input | Bias | Hidden | Output
type t

val init : ?value:float -> int -> kind -> t
val get_id : t -> int
val get_kind : t -> kind
val get_value : t -> float
