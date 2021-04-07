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

and mk_closure =
  { func_name : string
  ; free_vars : string list
  }

(* RHS of letrec binding is restricted to certain expr *)
and letrec_rhs =
  | Rhs_const of constant
  | Rhs_mkcls of mk_closure

type closure =
  { args : string list
  ; free_vars : string list
  ; body : expr
  }

(* A [t] is the closure intermediate representation (CIR) for a whole program. *)
type t =
  { funcs : (string, closure) Map.t
  ; expr : expr
  }

val from_ast_struct : Ast.structure -> t
