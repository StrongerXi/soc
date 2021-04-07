
(** A [t] represents a "name manager" for normal variables *)
type t

(** [init] is a bare minimum instance of t. *)
val init : t

(** [rename_struct t s] returns a new structure where all identifiers are
    consistenly renamed to some unique name (i.e., binding and usage synch) *)
val rename_struct : t -> Ast.structure -> (t * Ast.structure)

(** [gen_new_var_with_prefix t prefix] creates a variable name starting with
    [prefix], and is guaranteed to be unique in a context whose variables have
    been renamed with [t] *)
val gen_new_var_with_prefix : t -> string -> (t * string)
