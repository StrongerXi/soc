
(** A [t] represents a namer for type variables *)
type t

(** [init] is a bare minimum type variable namer. *)
val init : t

(** [rename_struct t s] returns a new structure where
    - all `Ast.Ty_var` are annotated to a unique new variable
    - same type variables in the same scope (defined by top-level `let`) are
      renamed the same way.
*)
val rename_struct : t -> Ast.structure -> (t * Ast.structure)

(** [gen_new_tyvar t] creates type variable name which is guaranteed to be
    unique in a context whose type variables have been renamed with [t] *)
val gen_new_tyvar : t -> (t * Ast.typ_desc)
