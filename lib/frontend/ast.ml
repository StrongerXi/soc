open Pervasives


type constant =
  | Const_Int of int
  | Const_Bool of bool

type typ =
  | Typ_const of string
      (* int, foobar, ... *)
  | Typ_var of string option
      (* 'a, 'b, ... [None] for _ *)
  | Typ_arrow of typ * typ
      (* int -> (int -> int) ... *)

type rec_flag =
  | Nonrecursive
  | Recursive

type opt_typed_var =
  { var : string
  ; typ : typ option
  } (* x : int *)

type expression =
  { expr_desc : expr_desc
  ; expr_span : Span.t
  ; expr_typ  : typ option (* (e : T), or filled in by typer *)
  }

and binding =
  { binding_lhs : opt_typed_var
  ; binding_rhs : expression
  } (* (x:int) = e *)

and expr_desc =
  | Exp_const of constant
  | Exp_ident of string
  | Exp_let of rec_flag * binding list * expression
        (* let (rec)? x1 = e1 and ... and xn = en in ebody, n > = 1 *)
  | Exp_fun of opt_typed_var list * expression
        (* fun (x0:int) x1 ... xn -> e, n >= 1*)
  | Exp_apply of expression * expression list
        (* (e0 e1 e2 ... en), n >= 1 *)
  | Exp_if of expression * expression * expression
        (* if e1 then e2 else e3 *)

type struct_item_desc =
  | Struct_eval of expression
        (* A single expression *)
  | Struct_bind of rec_flag * binding list
        (* let (rec)? x1 = e1 and ... and xn = en 
           Note that there is no body expression. *)

(** A [structure_item] is one integral item within a [structure] *)
type struct_item =
  { struct_item_desc : struct_item_desc;
    struct_item_span : Span.t;
  }

(** A [structure] represents the definition of a module. *)
type structure = struct_item list
