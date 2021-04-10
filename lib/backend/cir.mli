open Pervasives

type constant =
  | CInt of int
  | CBool of bool

type expr =
  | Cconst of constant
  | Cident of string
  | Cmk_closure of mk_closure
  | Clet of (string * expr) list * expr
  | Cletrec of (string * letrec_rhs) list * expr
  | Cif of expr * expr * expr
  | Cprimop of Primops.op_kind * expr list
  | Capply of expr * expr list

(** Creates a closure object while passing in [free_vars] as free variables
    required by the closure *)
and mk_closure =
  { func_name : string
  ; free_vars : string list
  }

(* RHS of letrec binding is restricted to certain expr *)
and letrec_rhs =
  | Rhs_const of constant
  | Rhs_mkcls of mk_closure

(** A [closure] represents a function that contains free/unbound variables
    which need to be supplied during construction of a closure object *)
type closure =
  { args : string list
  ; free_vars : string list
  ; body : expr
  }

(** A [prog] is the entire program at Cir level, which is represented as a
    single expression with functions as evaluation context. *)
type prog =
  { funcs : (string, closure) Map.t
  ; expr : expr
  }

(** [from_ast_struct struct] returns a CIR representation of [struct].
    ASSUME [struct] is well-typed based on Typer. *)
val from_ast_struct : Ast.structure -> prog
