type t
(** genome type *)

(* NOTE:
    - consider removing Random.State.t for perf increase
*)
val init : ?bias:bool -> int -> int -> Context.c -> t * Context.c
val copy : t          -> t
(* val mutate_weights : t            -> t Context.t *)
(* val mut_add_node   : t            -> t Context.t *)
(* val mut_add_connection : t            -> Context.t -> t * Context.t *)
val pp   : Format.formatter -> t -> unit
