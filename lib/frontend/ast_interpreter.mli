
(** A [context] is an immutable execution context for an AST interpreter *)
type context

(** An initial context that might contain some built-in things:
    - [print] function that prints to stdout and returns the input argument. *)
val initial_context : context

(** [interp_struct ctx struct] interprets [struct] in [ctx]; 
    returns the updated context. *)
val interp_struct : context -> Ast.structure -> context
