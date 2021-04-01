open Pervasives

(** A [t] represents the context during type inference. It
    - accumulates the errors encountered
    - maps normal variable to their current types, after unification(s) *)
type t

(** [empty] is an empty initial context *)
val empty : t

(** [add_error t err] returns a new context that keeps track of [err] *)
val add_error : t -> Errors.infer_error -> t

(** [get_errors t] returns a list of all errors obtained from [add_error],
    in ascending order of addition time *)
val get_errors : t -> Errors.infer_error list

(** [add_type t name typ] returns a new context that binds [name] with [typ],
    overwrite if [name] is already bound in [t] *)
val add_type : t -> string -> Ast.typ_desc -> t

(** [get_type t name span] returns the type bound to [name] in [t]; if not bound,
    a general type is returned to ensure continuation of typechecking, and an
    error is recorded in output context.  [span] is where the identifier [name]
    locates, used for error reporting. *)
val get_type : t -> string -> Span.t -> t * Ast.typ_desc


(* NOTE [unify_X] always accumulate error on failure, and return a general type
 * for contunuation of typechecking. *)

(** [unify t expect actual where] returns a new context in which [expect] and
    [actual] are unified to the same type; also returns the unified type. *)
val unify : t -> Ast.typ_desc -> Ast.typ_desc -> Span.t -> (t * Ast.typ_desc)

(** [unify_apply func_typ func_span arg_typ_span_pairs] returns output type. *)
val unify_apply : t
  -> Ast.typ_desc -> Span.t -> (Ast.typ_desc * Span.t) list
  -> (t * Ast.typ_desc)

(** [unify_binop binop lhs_typ lhs_span rhs_typ rhs_span] returns output type. *)
val unify_binop : t
  -> Ast.binary_op -> Ast.typ_desc -> Span.t -> Ast.typ_desc -> Span.t
  -> (t * Ast.typ_desc)


(** [generalize t name] generalizes the type bound to [name] in [t]. *)
val generalize : t -> string -> t


(** [rename_tyvars t s] returns an updated context and a structure that
    - has all `Ast.Ty_var` annotated to a unique new variable
    - same variables in the same scope (defined by top-level `let`) are renamed
      to the same variables *)
val rename_tyvars : t -> Ast.structure -> (t * Ast.structure)
