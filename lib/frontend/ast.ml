open Pervasives

type constant =
  | Int of int

type binary_op =
  | Add
  | Sub
  | Mul

type typ =
  { desc : typ_desc
  ; span : Span.t
  }

and typ_desc =
  | Typ_constant of string
      (* int, foobar, ... *)
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
  { desc : expr_desc
  ; span : Span.t
  }

and binding =
  { lhs : opt_typed_var
  ; rhs : expression
  } (* (x:int) = e *)

and expr_desc =
  | Exp_constant of constant
  | Exp_ident of string
  | Exp_binop of binary_op * expression * expression
  | Exp_let of rec_flag * binding list * expression
        (* let (rec)? x1 = e1 and ... and xn = en in ebody *)
  | Exp_fun of opt_typed_var list * expression
        (* fun (x0:int) x1 ... xn -> e, n >= 1*)
  | Exp_apply of expression * expression list
        (* (e0 e1 e2 ... en), n >= 1 *)
  | Pexp_ifthenelse of expression * expression * expression
        (* if e1 then e2 else e3 *)

type struct_item_desc =
  | Struct_eval of expression
        (* A single expression *)
  | Struct_bind of rec_flag * binding list
        (* let (rec)? x1 = e1 and ... and xn = en 
           Note that there is no body expression. *)

(** A [structure_item] is one integral item within a [structure] *)
type struct_item =
  { desc : struct_item_desc;
    span : Span.t;
  }

(** A [structure] represents the definition of a module. *)
type structure = struct_item list