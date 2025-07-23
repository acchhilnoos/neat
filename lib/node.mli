type kind = Input | Bias | Hidden | Output  (** node type type *)

type t
(** node type *)

val init : ?value:float -> int -> kind -> t
(** [init id kind] creates a node.

    [~value] allows the corresponding field to be set at creation (default=0.). *)

val get_id : t -> int
val get_kind : t -> kind
val get_value : t -> float
