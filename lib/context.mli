type c

type 'a t
(** context type *)

val init : unit -> c
val return : 'a -> 'a t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val run : 'a t -> c -> 'a * c
val fun_with : (c -> 'a * c) -> 'a t
val cget : int -> int -> int t
val nget : int t
val rand_int : int -> int t
val rand_float : float -> float t

module Let_syntax : sig
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end
