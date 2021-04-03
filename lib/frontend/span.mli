
(** A [t] represents a locational span in some source code file *)
type t =
  { start    : Location.t
  ; final    : Location.t
  }

(* [create start final] creates a [t] *)
val create : Location.t -> Location.t -> t

(** [merge s1 s2] returns a span that goes from the start of [s1] to
    the end of [s2]. *)
val merge : t -> t -> t
