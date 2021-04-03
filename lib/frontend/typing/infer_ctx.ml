open Pervasives

type scheme = (* a variation of type scheme from Hindley-Milner *)
  | Mono_typ of Ast.typ_desc
      (* a tyvar, to be resolved via substs *)
  | Poly_typ of string list * Ast.typ_desc
      (* tyvar params and body of the type scheme *)


(* INVARIANTS:
 * A. keys in [t.substs] never appear in values; graphically, this simulates a
 *    union-find structure, with tyvars being intermediate nodes, and typ_descs
 *    being root nodes (could itself be a tyvar, but can't appear in keys).
 * B. keys in [t.substs] never appear in typ_desc of [t.typ_env] (unless the key
 *    is one of tyvar params for the typ_desc); this means that types in the
 *    environment are always updated with substitutions. *)
type t =
  { cur_typ_env   : (string, scheme) Map.t        (* var name to scheme *)
  ; prev_typ_envs : (string, scheme) Map.t list   (* previous scopes *)
  ; substs        : (string, Ast.typ_desc) Map.t  (* tyvar name to type *)
  ; rev_errs      : Errors.infer_error list
  ; tv_namer      : Tyvar_namer.t
  }

let _get_new_tyvar t =
  let tv_namer, tyvar = Tyvar_namer.gen_new_tyvar t.tv_namer in
  let t = { t with tv_namer } in
  (t, tyvar)
;;

(* ENSURE: (a) output doesn't contain keys of [substs] *)
let rec _apply_substs_to_typ_desc
    (substs : (string, Ast.typ_desc) Map.t)  (desc : Ast.typ_desc)
  : Ast.typ_desc = 
  match desc with
  | Typ_const _ | Typ_var None -> desc
  | Typ_arrow (in_typ, out_typ) ->
    let in_typ  = _apply_substs_to_typ_desc substs in_typ in
    let out_typ = _apply_substs_to_typ_desc substs out_typ in
    Typ_arrow (in_typ, out_typ)
  | Typ_var (Some tv_name) ->
    let opt_desc = Map.get tv_name substs in (* Invariant (A) => (a) *)
    Option.value opt_desc desc
;;

(* replace each type param with unique tyvar, the source of polymorphism *)
let _scheme_to_typ_desc t (s : scheme) : (t * Ast.typ_desc) =
  match s with
  | Mono_typ typ_desc -> (t, typ_desc)
  | Poly_typ (ty_params, typ_desc) ->
    let t, substs = List.fold_left
        (fun (t, map) tyvar_param ->
           let t, tv_name = _get_new_tyvar t in
           let map = Map.add tyvar_param tv_name map in
           (t, map))
        (t, Map.empty String.compare)
        ty_params
    in
    let typ_desc = _apply_substs_to_typ_desc substs typ_desc in
    (t, typ_desc)
;;


(* ASSUME 
 * 1. [tyvar] and [typ] has gone been applied to by [t.substs], i.e.,
 *    (a). [tyvar] isn't a key of [t.substs] (['a : int] and ['a : bool] can't
 *         both appear in a substitution set.
 *    (b). [typ] doesn't contain keys of [t.substs]
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
  (* ENSURE (d). typ_desc in output doesn't contain [tyvar],
   *             unless [tyvar] ∈ [ty_params] *)
  let _subst_scheme (schm : scheme): scheme =
    match schm with
    | Mono_typ typ_desc -> Mono_typ (_subst_typ_desc typ_desc)
    | Poly_typ (ty_params, typ_desc) ->
      if List.mem tyvar ty_params then schm (* ty_params are fixed *)
      else Poly_typ (ty_params, _subst_typ_desc typ_desc)
  in
  let substs = Map.map _subst_typ_desc t.substs in
  let cur_typ_env = Map.map _subst_scheme t.cur_typ_env in
  let prev_typ_envs = List.map (Map.map _subst_scheme) t.prev_typ_envs in
  let substs = Map.add tyvar typ substs in
  (* for invariants, (b) & (c) & (2) ==> (A); (b) & (d) & (2) ==> (B) *)
  { t with substs; cur_typ_env; prev_typ_envs }
;;


let create tv_namer =
  { cur_typ_env   = Map.empty String.compare
  ; prev_typ_envs = []
  ; substs        = Map.empty String.compare
  ; rev_errs      = []
  ; tv_namer
  }
;;


let add_error t err =
  { t with rev_errs = err::t.rev_errs }
;;

let get_errors t =
  List.rev t.rev_errs
;;


let open_scope t =
  { t with cur_typ_env = Map.empty String.compare
         ; prev_typ_envs = t.cur_typ_env::t.prev_typ_envs }
;;

let close_scope t =
  match t.prev_typ_envs with
  | [] -> failwith "[Infer_ctx.close_scope] cannot close top level scope"
  | last::rest ->
    { t with cur_typ_env = last; prev_typ_envs = rest }
;;


let add_type t name typ =
  let typ = _apply_substs_to_typ_desc t.substs typ in
  let cur_typ_env = Map.add name (Mono_typ typ) t.cur_typ_env in
  { t with cur_typ_env }
;;

let get_type t name span =
  let rec go (scopes : (string, scheme) Map.t list) : (t * Ast.typ_desc) =
    match scopes with
    | [] -> 
      let err = Errors.Infer_unbound_var (name, span) in
      let t = add_error t err in
      let t, tyvar = _get_new_tyvar t in
      let t = add_type t name tyvar in
      (t, tyvar)
    | cur_scope::rest_scopes ->
      match Map.get name cur_scope with
      | None -> go rest_scopes
      | Some schm -> _scheme_to_typ_desc t schm
  in
  go (t.cur_typ_env::t.prev_typ_envs)
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

(* For easy error propogation back to [unify] *)
type _unify_error_reason =
  | Unify_mismatch 
  | Unify_occurs of string * Ast.typ_desc (* tyvar name and desc it occurs in *)
exception Unify_error of t * _unify_error_reason
let _unify_err t (err : _unify_error_reason) : 'a =
  raise (Unify_error (t, err))
;;

(* ASSUME [expect] and [actual] has been substituted to most specific form *)
let rec _unify t (expect : Ast.typ_desc) (actual : Ast.typ_desc) =
  let expect = _apply_substs_to_typ_desc t.substs expect in
  let actual = _apply_substs_to_typ_desc t.substs actual in
  match expect, actual with
  | Typ_var None, other | other, Typ_var None ->
    (t, other)
  | Typ_var (Some tv1), Typ_var (Some tv2) when tv1 = tv2 ->
    (t, actual)
  | Typ_var (Some tv), other | other, Typ_var (Some tv) ->
    if occurs tv other
    then _unify_err t (Unify_occurs (tv, other))
    else _add_subst t tv other, other
  | Typ_const ex, Typ_const ac when ex = ac -> (t, actual)
  | Typ_arrow (ex_in, ex_out), Typ_arrow (ac_in, ac_out) ->
    let t, in_typ = _unify t ex_in ac_in in
    let t, out_typ = _unify t ex_out ac_out in
    (t, Typ_arrow (in_typ, out_typ))
  | _ -> _unify_err t Unify_mismatch
;;

(* automatically catch and accumulate errors *)
let unify t expect actual actual_span =
  try _unify t expect actual
  with Unify_error (t, err) ->
    let expect = _apply_substs_to_typ_desc t.substs expect in
    let actual = _apply_substs_to_typ_desc t.substs actual in
    let err = match err with
      | Unify_mismatch ->
        Errors.Infer_type_mismatch (expect, actual, actual_span)
      | Unify_occurs (tv, occurree) ->
        Errors.Infer_tyvar_occurs (expect, actual, actual_span, tv, occurree)
    in
    (* update types based on the information gathered before mismatch *)
    let t = { t with rev_errs = err::t.rev_errs } in
    (t, actual)
;;

let unify_apply t func_typ func_span arg_typ_span_pairs =
  (* [f a b c]
   * --->
   * f : F = x_1 -> x_2 -> x_3 -> out
   * unify [F] [func_typ]
   * unify [x]_i [arg_typs]_i (so we have more accurate location info)
   * update and return [out] *)
  let t, out_ty = _get_new_tyvar t in
  let t, general_arg_typs, general_func_typ = 
    List.fold_right
      (fun _ (t, arg_typs, ret_typ) ->
         let t, arg_typ = _get_new_tyvar t in
         let ret_typ = Ast.Typ_arrow (arg_typ, ret_typ) in
         let arg_typs = arg_typ::arg_typs in
         (t, arg_typs, ret_typ))
      arg_typ_span_pairs
      (t, [], out_ty)
  in         (* can't error on general type *)
  let t, _ = unify t general_func_typ func_typ func_span in
  let t = List.fold_left
      (fun t (expect_typ, (actual_typ, actual_span)) ->
         let t, _ = unify t expect_typ actual_typ actual_span in t)
      t (List.combine general_arg_typs arg_typ_span_pairs)
  in
  let out_ty = _apply_substs_to_typ_desc t.substs out_ty in
  (t, out_ty)
;;

let _get_binop_typ (* returns the type of lhs, rhs, and output for [binop] *)
    t (binop : Ast.binary_op)
  : (t * Ast.typ_desc * Ast.typ_desc * Ast.typ_desc) =
  match binop with
  | Binop_add | Binop_sub | Binop_mul ->
    t, Builtin_types.int_typ, Builtin_types.int_typ, Builtin_types.int_typ
  | Binop_and | Binop_or ->
    t, Builtin_types.bool_typ, Builtin_types.bool_typ, Builtin_types.bool_typ
  | Binop_less ->
    t, Builtin_types.int_typ, Builtin_types.int_typ, Builtin_types.bool_typ
  | Binop_eq ->
    let (t, new_tyvar) = _get_new_tyvar t in
    t, new_tyvar, new_tyvar, Builtin_types.bool_typ
;;

let unify_binop t binop lhs_typ lhs_span rhs_typ rhs_span =
  let (t, lhs_expect, rhs_expect, out_ty) = _get_binop_typ t binop in
  let (t, _) = unify t lhs_expect lhs_typ lhs_span in
  let (t, _) = unify t rhs_expect rhs_typ rhs_span in
  let out_ty = _apply_substs_to_typ_desc t.substs out_ty in
  (t, out_ty)
;;



(* Some helpers for computing free type variables *)
let rec _add_fvs_in_typ_desc (s : string Set.t) (desc : Ast.typ_desc)
  : string Set.t =
  match desc with
  | Typ_const _ | Typ_var None -> s
  | Typ_var (Some tv) -> Set.add tv s
  | Typ_arrow (in_typ, out_typ) ->
    let s = _add_fvs_in_typ_desc s in_typ in
    _add_fvs_in_typ_desc s out_typ
;;

let _add_fvs_in_scheme (s : string Set.t) (schm : scheme) : string Set.t =
  match schm with
  | Mono_typ typ_desc -> _add_fvs_in_typ_desc s typ_desc
  | Poly_typ (ty_params, typ_desc) ->
    let fvs = _add_fvs_in_typ_desc s typ_desc in
    List.fold_right Set.remove ty_params fvs
;;

(* skip names in [names_to_ignore] *)
let _add_fvs_in_typ_env (s : string Set.t) (typ_env : (string, scheme) Map.t)
    (names_to_ignore : string Set.t) : string Set.t =
  Map.foldi (fun name schm s ->
      if Set.mem name names_to_ignore then s
      else _add_fvs_in_scheme s schm)
    typ_env s
;;

(* A helper for generalize 1 name in typ_env *)
let _generalize_typ_desc (typ_desc : Ast.typ_desc) (fvs_in_typ_env : string Set.t)
  : scheme =
  let fvs = Set.empty String.compare in
  let fvs = _add_fvs_in_typ_desc fvs typ_desc in
  let fvs = Set.diff fvs fvs_in_typ_env in
  let free_tyvars = Set.to_list fvs in
  (* INVARIANTS are preserved since we are adding more ty_params *)
  Poly_typ (free_tyvars, typ_desc)
;;

let generalize t names =
  (* ignore the names themselves in context, since their tyvars are not free *)
  let names_to_ignore =
    List.fold_right Set.add names (Set.empty String.compare) in
  let fvs_in_typ_env =
    List.fold_left
      (fun s typ_env -> _add_fvs_in_typ_env s typ_env names_to_ignore)
      (Set.empty String.compare) (t.cur_typ_env::t.prev_typ_envs)
  in
  List.fold_left
    (fun t name ->
       match Map.get name t.cur_typ_env with
       | None ->
         failwith "[Infer_ctx.generalize] can't generalize name unbound in current scope"
       | Some (Poly_typ _) ->
         failwith "[Infer_ctx.generalize] can't generalize same name multiple times"
       | Some (Mono_typ typ_desc) ->
         let generalized = _generalize_typ_desc typ_desc fvs_in_typ_env in
         { t with cur_typ_env = Map.add name generalized t.cur_typ_env })
    t names
;;
