open Pervasives

type scheme = (* a variation of type scheme from Hindley-Milner *)
  | Mono_typ of Ast.typ_desc
      (* a tyvar, to be resolved via substs *)
  | Poly_typ of string list * Ast.typ_desc
      (* tyvar params and body of the type scheme *)


type t =
  { typ_env  : (string, scheme) Map.t        (* var name to scheme or tyvar *)
  ; substs   : (string, Ast.typ_desc) Map.t  (* tyvar name to type *)
  ; rev_errs : Errors.infer_error list
  ; tv_namer : Tyvar_namer.t
  }

let _get_new_tyvar t =
  let tv_namer, tyvar = Tyvar_namer.gen_new_tyvar t.tv_namer in
  let t = { t with tv_namer } in
  (t, tyvar)
;;

let rec _map_typ_desc (* static map for each base case in [Ast.typ_desc] *)
    (f : Ast.typ_desc -> Ast.typ_desc)
    (desc : Ast.typ_desc) : Ast.typ_desc =
  match desc with
  | Typ_const _ -> f desc
  | Typ_var _ -> f desc
  | Typ_arrow (in_typ, out_typ) ->
    let in_typ = _map_typ_desc f in_typ in
    let out_typ = _map_typ_desc f out_typ in
    Typ_arrow (in_typ, out_typ)
;;

let rec _subst_typ_desc
    (substs : (string, Ast.typ_desc) Map.t)  (desc : Ast.typ_desc)
  : Ast.typ_desc = 
  match desc with (* TODO assume hitting non-tyvar is enough? *)
  | Typ_const _ | Typ_arrow _ | Typ_var None -> desc
  | Typ_var (Some tv_name) ->
    match Map.get tv_name substs with
    | None -> Ast.Typ_var (Some tv_name)
    | Some desc -> _subst_typ_desc substs desc
;;

let _scheme_to_typ_desc t (s : scheme) : (t * Ast.typ_desc) =
  match s with
  | Mono_typ typ_desc -> (t, _subst_typ_desc t.substs typ_desc)
  | Poly_typ (ty_params, typ_desc) ->
    let t, (map : (string, Ast.typ_desc) Map.t) = List.fold_left
        (fun (t, map) tyvar_param ->
           let t, tv_name = _get_new_tyvar t in
           let map = Map.add tyvar_param tv_name map in
           (t, map))
        (t, Map.empty String.compare)
        ty_params
    in
    let f (desc : Ast.typ_desc) = match desc with
      | Typ_const _ | Typ_arrow _ | Typ_var None -> desc
      | Typ_var (Some tyvar) ->
        match Map.get tyvar map with
        | None -> desc
        | Some desc -> desc
    in
    (t, _map_typ_desc f typ_desc)
;;


let rec _apply_subst_to_typ_desc (tyvar : string) (typ : Ast.typ_desc)
    (desc : Ast.typ_desc): Ast.typ_desc =
  match desc with
  | Typ_var (Some tv) when tv = tyvar -> typ
  | Typ_const _ | Typ_var _ -> desc
  | Typ_arrow (in_typ, out_typ) ->
    let in_typ = _apply_subst_to_typ_desc tyvar typ in_typ in
    let out_typ = _apply_subst_to_typ_desc tyvar typ out_typ in
    Typ_arrow (in_typ, out_typ)
;;

let _apply_subst_to_scheme (tyvar : string) (typ : Ast.typ_desc)
    (schm : scheme): scheme =
  match schm with
  | Mono_typ _ -> schm
  | Poly_typ (ty_params, typ_desc) ->
    if List.mem tyvar ty_params
    then schm
    else Poly_typ (ty_params, _apply_subst_to_typ_desc tyvar typ typ_desc)
;;

(* ASSUME [tyvar] and [typ] has been substituted into most specific form *)
let _apply_subst t (tyvar : string) (typ : Ast.typ_desc) : t =
  let substs = Map.map (_apply_subst_to_typ_desc tyvar typ) t.substs in
  let typ_env = Map.map (_apply_subst_to_scheme tyvar typ) t.typ_env in
  let substs = Map.add tyvar typ substs in
  { t with substs; typ_env }
;;


let create tv_namer =
  { typ_env   = Map.empty String.compare
  ; substs    = Map.empty String.compare
  ; rev_errs  = []
  ; tv_namer
  }
;;

let add_error t err =
  { t with rev_errs = err::t.rev_errs }
;;

let get_errors t =
  List.rev t.rev_errs
;;

let add_type t name typ =
  let typ_env = Map.add name (Mono_typ typ) t.typ_env in
  { t with typ_env }
;;

let get_type t name span =
  match Map.get name t.typ_env with
  | None -> 
      let err = Errors.Infer_unbound_var (name, span) in
      let t = add_error t err in
      let t, tyvar = _get_new_tyvar t in
      let t = add_type t name tyvar in
      (t, tyvar)
  | Some schm -> _scheme_to_typ_desc t schm
;;


exception Unify_mismatch of t
(* ASSUME [expect] and [actual] has been substituted to most specific form *)
let rec _unify t (expect : Ast.typ_desc) (actual : Ast.typ_desc) =
  let expect = _subst_typ_desc t.substs expect in
  let actual = _subst_typ_desc t.substs actual in
  let result =
    match expect, actual with
    | Typ_var None, other | other, Typ_var None ->
      (t, other)
    | Typ_var (Some tv), other | other, Typ_var (Some tv) ->
      (_apply_subst t tv other, other)
    | Typ_const ex, Typ_const ac when ex = ac -> (t, actual)
    | Typ_arrow (ex_in, ex_out), Typ_arrow (ac_in, ac_out) ->
      let t, in_typ = _unify t ex_in ac_in in
      let t, out_typ = _unify t ex_out ac_out in
      (t, Typ_arrow (in_typ, out_typ))
    (* TODO for nested mismatch errors, raise [error of t] and construct error in [unify] *)
    | _ -> raise (Unify_mismatch t)
  in
  result
;;

(* automatically catch and accumulate errors *)
let unify t expect actual where =
  try _unify t expect actual
  with Unify_mismatch t ->
    (* update types based on the information gathered before mismatch *)
    let expect = _subst_typ_desc t.substs expect in
    let actual = _subst_typ_desc t.substs actual in
    let err = Errors.Infer_type_mismatch (expect, actual, where) in
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
  let out_ty = _subst_typ_desc t.substs out_ty in
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
  let out_ty = _subst_typ_desc t.substs out_ty in
  (t, out_ty)
;;



let rec _add_fv_in_typ_desc (s : string Set.t) (desc : Ast.typ_desc)
  : string Set.t =
  match desc with
  | Typ_const _ | Typ_var None -> s
  | Typ_var (Some tv) -> Set.add tv s
  | Typ_arrow (in_typ, out_typ) ->
    let s = _add_fv_in_typ_desc s in_typ in
    _add_fv_in_typ_desc s out_typ
;;

let _sub_fv_in_scheme t (s : string Set.t) (schm : scheme) : string Set.t =
  match schm with
  | Mono_typ typ_desc ->
    let typ_desc = _subst_typ_desc t.substs typ_desc in
    let fvs = _add_fv_in_typ_desc (Set.empty String.compare) typ_desc in
    Set.diff s fvs
  | Poly_typ (ty_params, typ_desc) -> (* TODO need to subst typ_desc here? *)
    let fvs = _add_fv_in_typ_desc (Set.empty String.compare) typ_desc in
    let fvs = List.fold_right Set.remove ty_params fvs in
    Set.diff s fvs
;;

let _sub_fv_in_typ_env t (s : string Set.t) (typ_env : (string, scheme) Map.t)
  : string Set.t =
  Map.fold (fun schm s -> _sub_fv_in_scheme t s schm) typ_env s
;;

let generalize t name =
  match Map.get name t.typ_env with
  | None -> t (* ignore this request *)
  | Some (Poly_typ _) ->
    failwith "[Infer_ctx.generalize] can't generalize more than once"
  | Some (Mono_typ typ_desc) ->
    let typ_desc = _subst_typ_desc t.substs typ_desc in
    let s = Set.empty String.compare in
    let s = _add_fv_in_typ_desc s typ_desc in
    let typ_env = Map.remove name t.typ_env in (* NOTE critical *)
    let s = _sub_fv_in_typ_env t s typ_env in
    let free_tyvars = Set.to_list s in
    let schm = Poly_typ (free_tyvars, typ_desc) in
    { t with typ_env = Map.add name schm typ_env }
;;
