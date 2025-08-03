type c

type 'a t
(** context type *)

val init : unit -> c
val return : 'a -> 'a t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val run : 'a t -> c -> 'a * c
val cget : int -> int -> int t
val nget : int t
val rand_int : int -> c -> int
val rand_float : float -> c -> float
val m_rand_int : int -> int t
val m_rand_float : float -> float t

module Let_syntax : sig
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end
