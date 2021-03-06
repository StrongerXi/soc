open Pervasives

type scheme = (* type scheme from Hindley-Milner *)
  | Mono_typ of Ast.typ
  | Poly_typ of string list * Ast.typ
      (* tyvar params (DISTINCT) and body of the type scheme *)


(* INVARIANTS:
 * (A) The type environments are always updated with [t.substs] *)
type t =
  { cur_var_env   : (string, scheme) Map.t        (* var name to scheme *)
  ; prev_var_envs : (string, scheme) Map.t list   (* previous scopes *)
  ; substs        : Substs.t
  ; rev_errs      : Errors.typer_error list
  ; namer         : Namer.t
  }

let _get_new_tyvar t =
  let namer, new_name = Namer.gen_new_name t.namer in
  let tyvar = Ast.Typ_var (Some new_name) in
  let t = { t with namer } in
  (t, tyvar)
;;

(* replace each type param with unique tyvar, the source of polymorphism *)
let _scheme_to_typ t (s : scheme) : (t * Ast.typ) =
  match s with
  | Mono_typ typ -> (t, typ)
  | Poly_typ (ty_params, typ) ->
    let t, substs = List.fold_left
        (fun (t, substs) param ->
           let t, new_tyvar = _get_new_tyvar t in
           let param_tyvar = Ast.Typ_var (Some param) in
           (* can't error since ty_params are distinct *)
           let substs, _ = Substs.unify substs param_tyvar new_tyvar in
           (t, substs))
        (t, Substs.empty)
        ty_params
    in
    let typ = Substs.apply_to_typ substs typ in
    (t, typ)
;;

let _update_envs_with_substs t : t =
  let _update_scheme (schm : scheme): scheme =
    match schm with
    | Mono_typ typ -> Mono_typ (Substs.apply_to_typ t.substs typ)
    | Poly_typ (ty_params, typ) ->
      let substituted =
        Substs.apply_to_typ_exclude t.substs typ ty_params
      in Poly_typ (ty_params, substituted)
  in
  let cur_var_env = Map.map _update_scheme t.cur_var_env in
  let prev_var_envs = List.map (Map.map _update_scheme) t.prev_var_envs in
  { t with cur_var_env; prev_var_envs }
;;


let add_error t err =
  { t with rev_errs = err::t.rev_errs }
;;

let get_errors t =
  List.rev t.rev_errs
;;


let open_scope t =
  { t with cur_var_env   = String.empty_map ()
         ; prev_var_envs = t.cur_var_env::t.prev_var_envs }
;;

let close_scope t =
  match t.prev_var_envs with
  | [] -> failwith "[Typer_ctx.close_scope] cannot close top level scope"
  | last::rest ->
    { t with cur_var_env = last; prev_var_envs = rest }
;;


let add_type t name typ =
  let typ = Substs.apply_to_typ t.substs typ in (* INVARIANT (A) *)
  let cur_var_env = Map.add name (Mono_typ typ) t.cur_var_env in
  { t with cur_var_env }
;;

let get_type t name span =
  let rec go (scopes : (string, scheme) Map.t list) : (t * Ast.typ) =
    match scopes with
    | [] -> 
      let err = Errors.Typer_unbound_var (name, span) in
      let t = add_error t err in
      let t, tyvar = _get_new_tyvar t in
      let t = add_type t name tyvar in
      (t, tyvar)
    | cur_scope::rest_scopes ->
      match Map.get name cur_scope with
      | None -> go rest_scopes
      | Some schm -> _scheme_to_typ t schm
  in
  go (t.cur_var_env::t.prev_var_envs)
;;


(* automatically catch and accumulate errors *)
let unify t expect actual actual_span =
  let substs, err_opt = Substs.unify t.substs expect actual in
  let actual = Substs.apply_to_typ substs actual in
  let t = { t with substs } in
  match err_opt with
  | None -> let t = _update_envs_with_substs t in (t, expect)
  | Some err ->
    let expect = Substs.apply_to_typ substs expect in
    let err = match err with
      | Unify_mismatch ->
        Errors.Typer_type_mismatch (expect, actual, actual_span)
      | Unify_occurs (tv, occurree) ->
        Errors.Typer_tyvar_occurs (expect, actual, actual_span, tv, occurree)
    in
    (* update types based on the information gathered before mismatch *)
    let t = { t with rev_errs = err::t.rev_errs } in
    (t, expect)
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
  let out_ty = Substs.apply_to_typ t.substs out_ty in
  (t, out_ty)
;;


(* Some helpers for computing free type variables *)
let rec _add_fvs_in_typ (s : string Set.t) (desc : Ast.typ)
  : string Set.t =
  match desc with
  | Typ_const _ | Typ_var None -> s
  | Typ_var (Some tv) -> Set.add tv s
  | Typ_arrow (in_typ, out_typ) ->
    let s = _add_fvs_in_typ s in_typ in
    _add_fvs_in_typ s out_typ
;;

let _add_fvs_in_scheme (s : string Set.t) (schm : scheme) : string Set.t =
  match schm with
  | Mono_typ typ -> _add_fvs_in_typ s typ
  | Poly_typ (ty_params, typ) ->
    let fvs = _add_fvs_in_typ s typ in
    List.fold_right Set.remove ty_params fvs
;;

(* skip names in [names_to_ignore] *)
let _add_fvs_in_var_env (s : string Set.t) (var_env : (string, scheme) Map.t)
    (names_to_ignore : string Set.t) : string Set.t =
  Map.foldi (fun name schm s ->
      if Set.mem name names_to_ignore then s
      else _add_fvs_in_scheme s schm)
    var_env s
;;

(* A helper for generalize 1 name in var_env *)
let _generalize_typ (typ : Ast.typ) (fvs_in_var_env : string Set.t)
  : scheme =
  let fvs = String.empty_set in
  let fvs = _add_fvs_in_typ fvs typ in
  let fvs = Set.diff fvs fvs_in_var_env in
  let free_tyvars = Set.to_list fvs in
  (* INVARIANTS are preserved since we are adding more ty_params *)
  Poly_typ (free_tyvars, typ)
;;

let generalize t names =
  (* ignore the names themselves in context, since their tyvars are not free *)
  let names_to_ignore = Set.add_list names String.empty_set in
  let fvs_in_var_env =
    List.fold_left
      (fun s var_env -> _add_fvs_in_var_env s var_env names_to_ignore)
      String.empty_set (t.cur_var_env::t.prev_var_envs)
  in
  List.fold_left
    (fun t name ->
       match Map.get name t.cur_var_env with
       | None ->
         failwith "[Typer_ctx.generalize] can't generalize name unbound in current scope"
       | Some (Poly_typ _) ->
         failwith "[Typer_ctx.generalize] can't generalize same name multiple times"
       | Some (Mono_typ typ) ->
         let generalized = _generalize_typ typ fvs_in_var_env in
         { t with cur_var_env = Map.add name generalized t.cur_var_env })
    t names
;;


let update_typ t desc =
  Substs.apply_to_typ t.substs desc
;;

(* NOTE natives and primops will be declared in source once `external` syntax
 * is added *)
let _add_primops_into_var_env (var_env : (string, scheme) Map.t)
  : (string, scheme) Map.t =
  List.fold_left
    (fun env (info : Primops.op_info) ->
       let schm = _generalize_typ info.typ String.empty_set in
       Map.add info.opstr schm env)
    var_env Primops.all_op_infos
;;

let _add_natives_into_var_env (var_env : (string, scheme) Map.t)
  : (string, scheme) Map.t =
  let tyvar = Ast.Typ_var (Some "a") in
  let name_typ_pairs =
    [ (* these names must synch up with [Cir]'s natives *)
      ("=", Ast.Typ_arrow (tyvar, (Ast.Typ_arrow (tyvar, Builtin_types.bool_typ))));
      ("print", Ast.Typ_arrow (tyvar, tyvar));
    ]
    |> List.map (fun (name, typ) -> (name, _generalize_typ typ String.empty_set))
  in
  Map.add_pairs name_typ_pairs var_env 
;;

(* Initialize with some built-in stuff, an ad hoc solution *)
let create namer =
  let cur_var_env = String.empty_map () in
  (* It's okay to have custom tyvars in primops/natives, because we won't
   * generate new tyvars inside them anymore, and tyvar scope is limited to
   * each top-level *)
  let cur_var_env = _add_primops_into_var_env cur_var_env in
  let cur_var_env = _add_natives_into_var_env cur_var_env in
  { cur_var_env
  ; prev_var_envs = []
  ; substs        = Substs.empty
  ; rev_errs      = []
  ; namer
  }
;;
