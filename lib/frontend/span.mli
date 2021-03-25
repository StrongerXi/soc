
(** A [t] represents a locational span in some source code file *)
type t =
  { filename : string
  ; start    : Location.t
  ; final    : Location.t
  ; is_dummy : bool
  }

(** A dummy span generated during parsing *)
val dummy : t

(* [create filename start final] creates a [t] with [is_dummy = false] *)
val create : string -> Location.t -> Location.t -> t

(** [merge s1 s2] returns a span that goes from the start of [s1] to
    the end of [s2]. Errors if filenames mismatch. *)
val merge : t -> t -> t
