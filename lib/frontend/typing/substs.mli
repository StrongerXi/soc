open Pervasives

(** A [t] represents a "solution" to, or a set of substitutions that solves,
    some constraints (as equality among types), and we can map a type to another
    based on that solution, e.g.,

        { X1 = X2 -> int;
          X2 -> X4 = int -> X3; }

          has solution

        { X1 = int -> int;
          X2 = int
          X3 = X4 }
    
    Solution, or Substitution set, is computed incrementally as constraints are
    added, so the term [unify] is used instead of [add_constraint]. The former
    is also more familiar in the Hindley-Milner setting.

    Also note that we don't have to have concrete types on rhs of substitutions.
*)
type t

(** A bare minimum substitution set *)
val empty : t

(** [apply_to_typ t desc] applies the current substitutions to [desc] and
    return the resulting typ, e.g.,
        { X1 = int -> X3; X2 = bool; X4 = int } applied to [X1 -> X3] yields
        [(int -> X3) -> X3]
    ENSURE: output can't be changed via [t] anymore (it's canonicalized)
   *)
val apply_to_typ : t -> Ast.typ -> Ast.typ

(** Similar to [apply_to_typ], but the won't apply substitutions whose lhs
    is in the given string list *)
val apply_to_typ_exclude : t -> Ast.typ -> string list -> Ast.typ


(* [unify_error] is used to convey error encountered during a [unify] attempt *)
type unify_error =
  | Unify_mismatch                        (* the 2 types fail to match *)
  | Unify_occurs of string * Ast.typ (* tyvar and where it occurs in *)

(** [unify t typ1 typ2] tries to add [typ1 = typ2] as a constraint to [t].
    Regardless of whether an error is encountered, always return a [t] that
    contains any progress made through this call *)
val unify : t -> Ast.typ -> Ast.typ -> t * unify_error option
