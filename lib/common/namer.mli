
(** A [t] represents a "name manager" for renaming names in different data
    structures and generating unique new names. *)
type t

(** [init default_prefix] is a bare minimum instance of [t] that uses
    [default_prefix] to generate new names if prefix is not given. *)
val init : string -> t


(** [gen_new_name_with_prefix t prefix] creates a name which is guaranteed to
    - be unique in a context that has been renamed with [t] 
    - start with [prefix] *)
val gen_new_name_with_prefix : t -> string -> (t * string)

(** [gen_new_name t] is similar with [gen_new_name_with_prefix t prefix],
    but uses the default prefix passed in during [init] *)
val gen_new_name : t -> (t * string)


(** [rename_vars_in_ast_struct t s] returns a new structure where 
    - old names are used as prefix for new names
    - all bound identifiers are consistenly renamed to some unique name
      (i.e., binding and usage synch).
    NOTE unbound variables are left untouched. *)
val rename_vars_in_ast_struct : t -> Ast.structure -> (t * Ast.structure)

(** [rename_tyvars_in_ast_struct t s] returns a new structure where
    - all `Ast.Ty_var` without annotation are annotated to a unique tyvar.
    - old names are used as prefix for new names
    - same type variables in the same scope (defined by top-level `let`) are
      renamed the same way.  *)
val rename_tyvars_in_ast_struct : t -> Ast.structure -> (t * Ast.structure)
