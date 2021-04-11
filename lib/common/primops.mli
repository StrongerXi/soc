open Pervasives

(** A [t] represents some primitive infix operation *)
type op_kind =
  | AddInt
  | SubInt
  | MulInt
  | LogicAnd
  | LogicOr
  | LtInt

(** A [op_info] is all the information associated with a primitive operator *)
type op_info =
  { kind  : op_kind 
  ; typ   : Ast.typ
  ; opstr : string  (* its string representation as an operator *)
  ; label : string  (* unique label representation, no special symbols *)
  }


(** [all_ops] is a list of names for all the primitive operations.
    NOTE this is the single point of truth for all primitive operations *)
val all_op_infos : op_info list


(* ------Some query functions for convenience------ *)

(** [get_type s] returns the type of the primitive op associated with [s],
    or [None] if [s] is not a primitive op. *)
val get_type : string -> Ast.typ option

(** [get_op s] returns the primitive op associated with [s],
    or [None] if [s] is not a primitive op. *)
val get_kind : string -> op_kind option

(** [get_opstr op_kind] returns the opstr associated with [op_kind]. *)
val get_opstr : op_kind -> string
