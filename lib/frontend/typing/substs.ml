open Pervasives

(* INVARIANTS:
 * A. keys in [t] never appear in values; graphically, this simulates a
 *    union-find structure, with tyvars being intermediate nodes, and typ_descs
 *    being root nodes (could itself be a tyvar, but can't appear in keys). *)
type t = (string, Ast.typ_desc) Map.t

let empty = Map.empty String.compare


let rec apply_to_typ_desc_exclude t (desc : Ast.typ_desc) ignored =
  match desc with
  | Typ_const _ | Typ_var None -> desc
  | Typ_arrow (in_typ, out_typ) ->
    let in_typ  = apply_to_typ_desc_exclude t in_typ ignored in
    let out_typ = apply_to_typ_desc_exclude t out_typ ignored in
    Typ_arrow (in_typ, out_typ)
  | Typ_var (Some tv_name) ->
    if List.mem tv_name ignored then desc
    else
      let opt_desc = Map.get tv_name t in (* Invariant (A) is used (see mli) *)
      Option.value opt_desc desc
;;

let apply_to_typ_desc t (desc : Ast.typ_desc) =
  apply_to_typ_desc_exclude t desc []
;;


(* ASSUME 
 * 1. [tyvar] and [typ] has gone been applied to by [t.substs], i.e.,
 *    (a). [tyvar] isn't a key of [t] (['a : int] and ['a : bool] can't
 *         both appear in a substitution set.)
 *    (b). [typ] doesn't contain keys of [t]
 * 2. [tyvar] doesn't occur in [typ]. *)
let _add_subst t (tyvar : string) (typ : Ast.typ_desc) : t =
  (* ENSURE (c). output doesn't contain [tyvar] *)
  let rec _subst_typ_desc (desc : Ast.typ_desc): Ast.typ_desc =
    match desc with
    | Typ_var (Some tv) when tv = tyvar -> typ
    | Typ_const _ | Typ_var _ -> desc
    | Typ_arrow (in_typ, out_typ) ->
      let in_typ = _subst_typ_desc in_typ in
      let out_typ = _subst_typ_desc out_typ in
      Typ_arrow (in_typ, out_typ)
  in
  let t = Map.map _subst_typ_desc t in
  Map.add tyvar typ t
  (* for invariants, (b) & (c) & (2) ==> (A) *)
;;


(* does [tv_name] occur in [typ_desc]?
 * ASSUME [typ_desc] is updated with [t.substs] *)
let rec occurs (tv_name : string) (typ_desc : Ast.typ_desc) : bool =
  match typ_desc with
  | Typ_const _ | Typ_var None -> false
  | Typ_var (Some tv) -> tv = tv_name
  | Typ_arrow (in_typ, out_typ) ->
    (occurs tv_name in_typ) || (occurs tv_name out_typ)
;;

type unify_error =
  | Unify_mismatch                        
  | Unify_occurs of string * Ast.typ_desc

(* For easy error propogation back to [unify] *)
exception Unify_error of t * unify_error
let _unify_err t (err : unify_error) : 'a =
  raise (Unify_error (t, err))
;;

(* XXX return resulting type could speed this up a bit, but I'm more concerned
 * with design for now *)
let rec _unify t (expect : Ast.typ_desc) (actual : Ast.typ_desc) : t =
  let expect = apply_to_typ_desc t expect in
  let actual = apply_to_typ_desc t actual in
  match expect, actual with
  | Typ_var None, _ | _, Typ_var None -> t
  | Typ_var (Some tv1), Typ_var (Some tv2) when tv1 = tv2 -> t
  | Typ_var (Some tv), other | other, Typ_var (Some tv) ->
    if occurs tv other
    then _unify_err t (Unify_occurs (tv, other))
    else _add_subst t tv other
  | Typ_const ex, Typ_const ac when ex = ac -> t
  | Typ_arrow (ex_in, ex_out), Typ_arrow (ac_in, ac_out) ->
    let t = _unify t ex_in ac_in in
    _unify t ex_out ac_out
  | _ -> _unify_err t Unify_mismatch
;;

let unify t expect actual =
  try (_unify t expect actual, None)
  with Unify_error (t, err) -> (t, Some err)
;;
