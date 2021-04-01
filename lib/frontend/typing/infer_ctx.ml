open Pervasives

type scheme = (* a variation of type scheme from Hindley-Milner *)
  | Mono_typ of string
      (* a tyvar, to be resolved via substs *)
  | Poly_typ of string list * Ast.typ_desc
      (* tyvar params and body of the type scheme *)


type t =
  { cur_scope   : (string, scheme) Map.t        (* var name to scheme or tyvar *)
  ; prev_scopes : ((string, scheme) Map.t) list (* var name to scheme or tyvar *)
  ; substs      : (string, Ast.typ_desc) Map.t  (* tyvar name to type *)
  ; rev_errs    : Errors.infer_error list
  ; stamp       : int                           (* for unique name generation *)
  }


let _get_new_tyvar_name t : (t * string) =
  let name = String.append "X" (Int.to_string t.stamp) in
  let t = { t with stamp = t.stamp + 1 } in
  (t, name)
;;

let _get_new_tyvar t : (t * Ast.typ_desc) =
  let t, name = _get_new_tyvar_name t in
  let tyvar = Ast.Typ_var (Some name) in
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

let rec _subst_tv_name (substs : (string, Ast.typ_desc) Map.t) (tv : string)
  : Ast.typ_desc =
  match Map.get tv substs with
  | None -> Ast.Typ_var (Some tv)
  | Some desc -> _subst_typ_desc substs desc

and _subst_typ_desc
    (substs : (string, Ast.typ_desc) Map.t)  (desc : Ast.typ_desc)
  : Ast.typ_desc = 
  match desc with (* TODO assume hitting non-tyvar is enough? *)
  | Typ_const _ | Typ_arrow _ | Typ_var None -> desc
  | Typ_var (Some tyvar) -> _subst_tv_name substs tyvar
;;

let _scheme_to_typ_desc t (s : scheme) : (t * Ast.typ_desc) =
  match s with
  | Mono_typ tyvar -> (t, _subst_tv_name t.substs tyvar)
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

let _get_scheme t name : scheme option =
  let rec go scopes = 
    match scopes with
    | [] -> None
    | scp::scopes ->
      match Map.get name scp with
      | None -> go scopes
      | Some schm -> Some schm
  in
  go (t.cur_scope::t.prev_scopes)
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
  let cur_scope = Map.map (_apply_subst_to_scheme tyvar typ) t.cur_scope in
  let prev_scopes =
    List.map
      (fun scope -> Map.map (_apply_subst_to_scheme tyvar typ) scope)
      t.prev_scopes
  in
  let substs = Map.add tyvar typ substs in
  { t with substs; cur_scope; prev_scopes }
;;


let empty =
  { cur_scope   = Map.empty String.compare
  ; prev_scopes = []
  ; substs      = Map.empty String.compare
  ; rev_errs    = []
  ; stamp       = 0
  }
;;

let add_error t err =
  { t with rev_errs = err::t.rev_errs }
;;

let get_errors t =
  List.rev t.rev_errs
;;

let add_type t name typ =
  let t, tv_name = _get_new_tyvar_name t in
  let substs = Map.add tv_name typ t.substs in
  let cur_scope = Map.add name (Mono_typ tv_name) t.cur_scope in
  { t with substs; cur_scope }
;;

let get_type t name span =
  match _get_scheme t name with
  | None -> 
      let err = Errors.Infer_unbound_var (name, span) in
      let t = add_error t err in
      let t, tyvar = _get_new_tyvar t in
      let t = add_type t name tyvar in
      (t, tyvar)
  | Some schm -> _scheme_to_typ_desc t schm
;;

let open_scope t = 
  { t with cur_scope = Map.empty String.compare
         ; prev_scopes = t.cur_scope::t.prev_scopes }
;;

let close_scope t =
  match t.prev_scopes with
  | [] -> failwith "[Infer_ctx.close_scope] can't close global scope"
  | prev::prev_scopes -> { t with cur_scope = prev; prev_scopes }
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
  | Mono_typ tv_name ->
    let typ_desc = _subst_tv_name t.substs tv_name in
    let fvs = _add_fv_in_typ_desc (Set.empty String.compare) typ_desc in
    Set.diff s fvs
  | Poly_typ (ty_params, typ_desc) -> (* TODO need to subst typ_desc here? *)
    let fvs = _add_fv_in_typ_desc (Set.empty String.compare) typ_desc in
    let fvs = List.fold_right Set.remove ty_params fvs in
    Set.diff s fvs
;;

let _sub_fv_in_scope t (s : string Set.t) (scope : (string, scheme) Map.t)
  : string Set.t =
  Map.fold (fun schm s -> _sub_fv_in_scheme t s schm) scope s
;;

let rec _remove_first_from_scopes (name : string)
    (scopes : (string, scheme) Map.t list) : (string, scheme) Map.t list =
  match scopes with
  | [] -> []
  | scp::scopes ->
    match Map.get name scp with
    | None -> scp::(_remove_first_from_scopes name scopes)
    | _ -> (Map.remove name scp)::scopes
;;

let generalize t name =
  match _get_scheme t name with
  | None -> t (* ignore this request *)
  | Some (Poly_typ _) ->
    failwith "[Infer_ctx.generalize] can't generalize more than once"
  | Some (Mono_typ tv_name) ->
    let typ_desc = _subst_tv_name t.substs tv_name in
    let s = Set.empty String.compare in
    let s = _add_fv_in_typ_desc s typ_desc in
    let scopes = t.cur_scope::t.prev_scopes in
    let scopes = _remove_first_from_scopes name scopes in (* NOTE critical *)
    let s = List.fold_left (fun s scp -> _sub_fv_in_scope t s scp) s scopes in
    let free_tyvars = Set.to_list s in
    let schm = Poly_typ (free_tyvars, typ_desc) in
    { t with cur_scope = Map.add name schm t.cur_scope }
;;


let rec _rename_tyvars_in_typ_desc t (tv_map : (string, string) Map.t)
    (desc : Ast.typ_desc) : (t * (string, string) Map.t * Ast.typ_desc) =
  match desc with
  | Typ_const _ -> (t, tv_map, desc)
  | Typ_var None -> (* [_] becomes a unique new tyvar *)
    let t, desc = _get_new_tyvar t in
    (t, tv_map, desc)
  | Typ_var (Some tv) ->
    begin
      match Map.get tv tv_map with
      | None -> (* first occurrence introduces binding implicitly *)
        let t, new_tv = _get_new_tyvar_name t in
        let tv_map = Map.add tv new_tv tv_map in
        let desc = Ast.Typ_var (Some new_tv) in
        (t, tv_map, desc)
      | Some (new_tv) ->
        let desc = Ast.Typ_var (Some new_tv) in
        (t, tv_map, desc)
    end
  | Typ_arrow (in_typ, out_typ) ->
    let t, tv_map, in_typ = _rename_tyvars_in_typ_desc t tv_map in_typ in
    let t, tv_map, out_typ = _rename_tyvars_in_typ_desc t tv_map out_typ in
    let desc = Ast.Typ_arrow (in_typ, out_typ) in
    (t, tv_map, desc)
;;

let _rename_tyvars_in_opt_typed_var t (tv_map : (string, string) Map.t)
    (otv : Ast.opt_typed_var) : (t * (string, string) Map.t * Ast.opt_typed_var) =
  let t, typ, tv_map = match otv.typ with
    | None -> (* [x] becomes [(x : _)] *)
      let desc = Ast.Typ_var None in
      let t, tv_map, typ_desc = _rename_tyvars_in_typ_desc t tv_map desc in
      let typ = { Ast.typ_desc; typ_span = Span.dummy } in
      (t, typ, tv_map)
    | Some typ -> 
      let t, tv_map, typ_desc = _rename_tyvars_in_typ_desc t tv_map typ.typ_desc in
      let typ = { typ with Ast.typ_desc; } in
      (t, typ, tv_map)
  in
  let otv = { otv with typ = Some typ } in
  (t, tv_map, otv)
;;

let rec _rename_tyvars_in_expr t (tv_map : (string, string) Map.t)
    (expr : Ast.expression) : (t * Ast.expression) =
  match expr.expr_desc with
  | Exp_const _ | Exp_ident _  -> (t, expr)
  | Exp_fun (names, body) ->
    let t, tv_map, rev_names = List.fold_left
        (fun (t, tv_map, rev_names) otv ->
           let t, tv_map, name = _rename_tyvars_in_opt_typed_var t tv_map otv in
           (t, tv_map, name::rev_names))
        (t, tv_map, [])
        names
    in
    let names = List.rev rev_names in
    let t, body = _rename_tyvars_in_expr t tv_map body in
    let expr = { expr with expr_desc = Exp_fun (names, body) } in
    (t, expr)
  | Exp_binop (binop, lhs, rhs) ->
    let t, lhs = _rename_tyvars_in_expr t tv_map lhs in
    let t, rhs = _rename_tyvars_in_expr t tv_map rhs in
    let expr = { expr with expr_desc = Exp_binop (binop, lhs, rhs) } in
    (t, expr)
  | Exp_if (cnd, thn, els) ->
    let t, cnd = _rename_tyvars_in_expr t tv_map cnd in
    let t, thn = _rename_tyvars_in_expr t tv_map thn in
    let t, els = _rename_tyvars_in_expr t tv_map els in
    let expr = { expr with expr_desc = Exp_if (cnd, thn, els) } in
    (t, expr)
  | Exp_let (rec_flag, bds, body) ->
    let t, tv_map, bds = _rename_tyvars_in_let_bindings t tv_map bds in
    let t, body = _rename_tyvars_in_expr t tv_map body in
    let expr = { expr with expr_desc = Exp_let (rec_flag, bds, body) } in
    (t, expr)
  | Exp_apply (func, args) -> 
    let t, func = _rename_tyvars_in_expr t tv_map func in
    let t, rev_args = List.fold_left
        (fun (t, rev_args) arg ->
           let t, arg = _rename_tyvars_in_expr t tv_map arg in
           (t, arg::rev_args))
        (t, [])
        args
    in
    let args = List.rev rev_args in
    let expr = { expr with expr_desc = Exp_apply (func, args) } in
    (t, expr)

and _rename_tyvars_in_let_bindings t (tv_map : (string, string) Map.t)
    (bds : Ast.binding list)
  : (t * (string, string) Map.t * Ast.binding list) =

  let _rename_tyvars_in_one_binding t (tv_map : (string, string) Map.t)
     (bd : Ast.binding) : (t * (string, string) Map.t * Ast.binding) =
    let t, tv_map, binding_lhs =
      _rename_tyvars_in_opt_typed_var t tv_map bd.binding_lhs in
    let t, binding_rhs = _rename_tyvars_in_expr t tv_map bd.binding_rhs in
    let bd = { Ast.binding_lhs; binding_rhs } in
    (t, tv_map, bd)
  in
  let t, tv_map, rev_bds = List.fold_left
      (fun (t, tv_map, rev_bds) bd ->
         let t, tv_map, bd = _rename_tyvars_in_one_binding t tv_map bd in
         (t, tv_map, bd::rev_bds))
      (t, tv_map, [])
      bds
  in
  let bds = List.rev rev_bds in
  (t, tv_map, bds)
;;

let _rename_tyvars_in_struct_item t (tv_map : (string, string) Map.t)
    (item : Ast.struct_item) : (t * (string, string) Map.t * Ast.struct_item) =
  match item.struct_item_desc with
  | Struct_eval expr -> 
    let (t, expr) = _rename_tyvars_in_expr t tv_map expr in
    let item = { item with struct_item_desc = Struct_eval expr } in
    (t, tv_map, item)
  | Struct_bind (rec_flag, bds) ->
    let (t, tv_map, bds) = _rename_tyvars_in_let_bindings t tv_map bds in
    let item = { item with struct_item_desc = Struct_bind (rec_flag, bds) } in
    (t, tv_map, item)
;;

let rename_tyvars t structure =
  let t, _, rev_struct_items = List.fold_left
      (fun (t, tv_map, rev_struct_items) item ->
         let t, tv_map, item = _rename_tyvars_in_struct_item t tv_map item in
         (t, tv_map, item::rev_struct_items))
      (t, Map.empty String.compare, [])
      structure
  in
  (t, List.rev rev_struct_items)
;;
