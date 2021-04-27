open Pervasives

(** A [t] represents the context during type inference. It
    - accumulates the errors encountered
    - maps normal variable to their current types, after unification(s) 
    - keeps track of different levels of scope *)
type t

(** [create namer] is an initial context which will use [namer] to
    generate new tyvars in following operations. *)
val create : Namer.t -> t

(** [add_error t err] returns a new context that keeps track of [err] *)
val add_error : t -> Errors.typer_error -> t

(** [get_errors t] returns a list of all errors obtained from [add_error],
    in ascending order of addition time *)
val get_errors : t -> Errors.typer_error list

(** [open_scope t] returns a context with a new scope opened *)
val open_scope : t -> t

(** [close_scope t] removes all bindings added in the current scope *)
val close_scope : t -> t

(** [add_type t name typ] returns a new context that binds [name] with [typ] in
 * the current scope, overwrite if [name] is already bound there. *)
val add_type : t -> string -> Ast.typ -> t

(** [get_type t name span] returns the type bound to [name] in [t], with type
    appropriate instantiation if [name] has been generalized (so it behaves
    polymorphically when upon unification).
    If [name] is not bound in [t], a general type is returned to ensure
    continuation of typechecking, and an error is recorded in output context.
    [span] is where the identifier [name] locates, used for error reporting. *)
val get_type : t -> string -> Span.t -> t * Ast.typ


(* NOTE [unify_X] always accumulate error on failure, and return a general type
 * for contunuation of typechecking. *)

(** [unify t expect actual where] returns a new context in which [expect] and
    [actual] are unified to the same type; also returns the unified type. *)
val unify : t -> Ast.typ -> Ast.typ -> Span.t -> (t * Ast.typ)

(** [unify_apply func_typ func_span arg_typ_span_pairs] returns output type. *)
val unify_apply : t
  -> Ast.typ -> Span.t -> (Ast.typ * Span.t) list -> (t * Ast.typ)


(** [generalize t names] generalizes the types bound to [names] in [t], so that
    they might behave as polymorphic types.
    ASSUME [names] are bound in current scope of [t], not previous ones. *)
val generalize : t -> string list -> t


(** Some old typ might be out of synch now (after some unification calls)
    since they are immutable. This provides a way to "synch them up". *)
val update_typ : t -> Ast.typ -> Ast.typ
