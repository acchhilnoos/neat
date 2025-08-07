type t
(** genome type *)

(* NOTE:
    - consider removing Random.State.t for perf increase
*)
val init : ?bias:bool -> ?st:Context.c -> int -> int -> t * Context.c
val copy : t -> t
val add_node : t -> t Context.t
val add_connection : t -> t Context.t
val pp : Format.formatter -> t -> unit
