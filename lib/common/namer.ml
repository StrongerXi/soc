open Pervasives

type t = (* information for generating unique names *)
  { stamp            : int
  ; default_prefix   : string
  }

let init default_prefix =
  { stamp = 0
  ; default_prefix
  }
;;

let gen_new_name_with_prefix t prefix =
  let name = String.concat [prefix; "_"; Int.to_string t.stamp] in
  let t = { t with stamp = t.stamp + 1 } in
  (t, name)
;;

let gen_new_name t =
  gen_new_name_with_prefix t t.default_prefix
;;


type rename_context =
  { namer            : t
  ; curr_scope       : (string, string) Map.t
      (* we don't really need curr_scope, but it simplifies implementation *)
  ; prev_scopes      : ((string, string) Map.t) list
      (* essentially persistent old "versions" of [curr_scope] *)
  }

let _init_ctx t =
  { namer            = t
  ; curr_scope       = Map.empty String.compare
  ; prev_scopes      = []
  }

let _ctx_gen_new_name (ctx : rename_context) : (rename_context * string) =
  let namer, new_name  = gen_new_name ctx.namer in
  let ctx = { ctx with namer } in
  (ctx, new_name)
;;

let _ctx_gen_and_bind_new_var (ctx : rename_context) (old_name : string)
  : (rename_context * string) =
  let namer, new_name  = gen_new_name_with_prefix ctx.namer old_name in
  let ctx = { ctx with namer;
                       curr_scope = Map.add old_name new_name ctx.curr_scope;
            } in
  (ctx, new_name)
;;

let _ctx_open_scope (ctx : rename_context) : rename_context =
  { ctx with
    prev_scopes = ctx.curr_scope::ctx.prev_scopes;
  }
;;

let _ctx_close_scope (ctx : rename_context) : rename_context =
  match ctx.prev_scopes with
  | [] -> failwith "[Namer._ctx_close_scope] Cannot close global scope"
  | prev::prevs ->
    { ctx with
      curr_scope = prev;
      prev_scopes = prevs;
    }
;;

let _ctx_get_new_name_if_bound (ctx : rename_context) (old_name : string)
  : (rename_context * string) =
  match Map.get old_name ctx.curr_scope with
  | None -> (ctx, old_name)
  | Some new_name -> (ctx, new_name)
;;

let _ctx_get_or_bind_new_name (ctx : rename_context) (old_name : string)
  : (rename_context * string) =
  match Map.get old_name ctx.curr_scope with
  | None -> _ctx_gen_and_bind_new_var ctx old_name
  | Some new_name -> (ctx, new_name)
;;


let _rename_vars_in_opt_typed_var (ctx : rename_context) (otv : Ast.opt_typed_var)
  : (rename_context * Ast.opt_typed_var) =
  (* binding always makes a new variable *)
  let old_name = otv.var in
  let ctx, new_name = _ctx_gen_and_bind_new_var ctx old_name in
  ctx, { otv with var = new_name }
;;

let rec _rename_vars_in_expr (ctx : rename_context)
    (expr : Ast.expression) : (rename_context * Ast.expression) =
  match expr.expr_desc with
  | Exp_const _ -> (ctx, expr)
  | Exp_ident old_name -> 
    let ctx, new_name = _ctx_get_new_name_if_bound ctx old_name in
    let expr = { expr with expr_desc = Exp_ident new_name } in
    (ctx, expr)

  | Exp_fun (names, body) ->
    let ctx = _ctx_open_scope ctx in
    let ctx, rev_names = List.fold_left
        (fun (ctx, rev_names) otv ->
           let ctx, name = _rename_vars_in_opt_typed_var ctx otv in
           (ctx, name::rev_names))
        (ctx, [])
        names
    in
    let names = List.rev rev_names in
    let ctx, body = _rename_vars_in_expr ctx body in
    let ctx = _ctx_close_scope ctx in
    let expr = { expr with expr_desc = Exp_fun (names, body) } in
    (ctx, expr)

  | Exp_if (cnd, thn, els) ->
    let ctx, cnd = _rename_vars_in_expr ctx cnd in
    let ctx, thn = _rename_vars_in_expr ctx thn in
    let ctx, els = _rename_vars_in_expr ctx els in
    let expr = { expr with expr_desc = Exp_if (cnd, thn, els) } in
    (ctx, expr)

  | Exp_let (rec_flag, bds, body) ->
    let ctx = _ctx_open_scope ctx in
    let ctx, bds = _rename_vars_in_let_bindings ctx rec_flag bds in
    let ctx, body = _rename_vars_in_expr ctx body in
    let ctx = _ctx_close_scope ctx in
    let expr = { expr with expr_desc = Exp_let (rec_flag, bds, body) } in
    (ctx, expr)

  | Exp_apply (func, args) -> 
    let ctx, func = _rename_vars_in_expr ctx func in
    let ctx, rev_args = List.fold_left
        (fun (ctx, rev_args) arg ->
           let ctx, arg = _rename_vars_in_expr ctx arg in
           (ctx, arg::rev_args))
        (ctx, [])
        args
    in
    let args = List.rev rev_args in
    let expr = { expr with expr_desc = Exp_apply (func, args) } in
    (ctx, expr)

and _rename_vars_in_let_bindings init_ctx
    (rec_flag : Ast.rec_flag) (init_bds : Ast.binding list)
  : (rename_context * Ast.binding list) =

  let rename_all_bd_lhs ctx bds : (rename_context * Ast.binding list) =
    List.fold_right
      (fun (bd : Ast.binding) (ctx, bds) ->
         let ctx, binding_lhs =
           _rename_vars_in_opt_typed_var ctx bd.binding_lhs in
         let bd = { bd with binding_lhs } in
         (ctx, bd::bds))
      bds (ctx, [])
  in

  let rename_all_bd_rhs ctx bds : (rename_context *  Ast.binding list) =
    List.fold_right
      (fun (bd : Ast.binding) (ctx, bds) ->
         let ctx, binding_rhs = _rename_vars_in_expr ctx bd.binding_rhs in
         let bd = { bd with binding_rhs } in
         (ctx, bd::bds))
      bds (ctx, [])
  in

  match rec_flag with (* based on the scoping rules of [let] vs [let rec] *)
  | Nonrecursive ->
    let ctx_with_rhs, bds = rename_all_bd_rhs init_ctx init_bds in
    rename_all_bd_lhs ctx_with_rhs bds

  | Recursive ->
    let ctx_with_lhs, bds = rename_all_bd_lhs init_ctx init_bds in
    let ctx_with_bds, bds = rename_all_bd_rhs ctx_with_lhs bds in
    (ctx_with_bds, bds)
;;

let _rename_vars_in_struct_item (ctx : rename_context) (item : Ast.struct_item)
  : (rename_context * Ast.struct_item) =
  match item.struct_item_desc with
  | Struct_eval expr -> 
    let ctx, expr = _rename_vars_in_expr ctx expr in
    let item = { item with struct_item_desc = Struct_eval expr } in
    (ctx, item)

  | Struct_bind (rec_flag, bds) ->
    let ctx, bds = _rename_vars_in_let_bindings ctx rec_flag bds in
    let item = { item with struct_item_desc = Struct_bind (rec_flag, bds) } in
    (ctx, item)
;;

let rename_vars_in_ast_struct t structure =
  let ctx = _init_ctx t in
  let ctx, rev_struct_items = List.fold_left
      (fun (ctx, rev_struct_items) item ->
         let ctx, item = _rename_vars_in_struct_item ctx item in
         (ctx, item::rev_struct_items))
      (ctx, [])
      structure
  in
  (ctx.namer, List.rev rev_struct_items)
;;


let rec _rename_tyvars_in_typ (ctx : rename_context) (desc : Ast.typ)
  : (rename_context * Ast.typ) =
  match desc with
  | Typ_const _ -> (ctx, desc)
  | Typ_var None -> (* [_] becomes a unique new tyvar *)
    let ctx, new_name = _ctx_gen_new_name ctx in
    let tyvar = Ast.Typ_var (Some new_name) in
    (ctx, tyvar)

  | Typ_var (Some tv) ->
    let ctx, new_tv = _ctx_get_or_bind_new_name ctx tv in
    let desc = Ast.Typ_var (Some new_tv) in
    (ctx, desc)

  | Typ_arrow (in_typ, out_typ) ->
    let ctx, in_typ = _rename_tyvars_in_typ ctx in_typ in
    let ctx, out_typ = _rename_tyvars_in_typ ctx out_typ in
    let desc = Ast.Typ_arrow (in_typ, out_typ) in
    (ctx, desc)
;;

let _rename_tyvars_in_opt_typed_var
    (ctx : rename_context) (otv : Ast.opt_typed_var)
  : (rename_context * Ast.opt_typed_var) =
  let typ = (* [x] becomes [(x : _)] *)
    match otv.typ with 
    | None     -> Ast.Typ_var None
    | Some typ -> typ
  in
  let ctx, typ = _rename_tyvars_in_typ ctx typ in
  let otv = { otv with typ = Some typ } in
  (ctx, otv)
;;

let rec _rename_tyvars_in_expr (ctx : rename_context) (expr : Ast.expression)
  : (rename_context * Ast.expression) =
  match expr.expr_desc with
  | Exp_const _ | Exp_ident _  -> (ctx, expr)

  | Exp_fun (names, body) ->
    let ctx, rev_names = List.fold_left
        (fun (ctx, rev_names) otv ->
           let ctx, name = _rename_tyvars_in_opt_typed_var ctx otv in
           (ctx, name::rev_names))
        (ctx, [])
        names
    in
    let names = List.rev rev_names in
    let ctx, body = _rename_tyvars_in_expr ctx body in
    let expr = { expr with expr_desc = Exp_fun (names, body) } in
    (ctx, expr)

  | Exp_if (cnd, thn, els) ->
    let ctx, cnd = _rename_tyvars_in_expr ctx cnd in
    let ctx, thn = _rename_tyvars_in_expr ctx thn in
    let ctx, els = _rename_tyvars_in_expr ctx els in
    let expr = { expr with expr_desc = Exp_if (cnd, thn, els) } in
    (ctx, expr)

  | Exp_let (rec_flag, bds, body) ->
    let ctx, bds = _rename_tyvars_in_let_bindings ctx bds in
    let ctx, body = _rename_tyvars_in_expr ctx body in
    let expr = { expr with expr_desc = Exp_let (rec_flag, bds, body) } in
    (ctx, expr)

  | Exp_apply (func, args) -> 
    let ctx, func = _rename_tyvars_in_expr ctx func in
    let ctx, rev_args = List.fold_left
        (fun (ctx, rev_args) arg ->
           let ctx, arg = _rename_tyvars_in_expr ctx arg in
           (ctx, arg::rev_args))
        (ctx, [])
        args
    in
    let args = List.rev rev_args in
    let expr = { expr with expr_desc = Exp_apply (func, args) } in
    (ctx, expr)

and _rename_tyvars_in_let_bindings
    (ctx : rename_context) (bds : Ast.binding list)
  : (rename_context * Ast.binding list) =

  let _rename_tyvars_in_one_binding (ctx : rename_context)
     (bd : Ast.binding) : (rename_context * Ast.binding) =
    let ctx, binding_lhs =
      _rename_tyvars_in_opt_typed_var ctx bd.binding_lhs in
    let ctx, binding_rhs = _rename_tyvars_in_expr ctx bd.binding_rhs in
    let bd = { Ast.binding_lhs; binding_rhs } in
    (ctx, bd)
  in

  let ctx, rev_bds = List.fold_left
      (fun (ctx, rev_bds) bd ->
         let ctx, bd = _rename_tyvars_in_one_binding ctx bd in
         (ctx, bd::rev_bds))
      (ctx, [])
      bds
  in
  let bds = List.rev rev_bds in
  (ctx, bds)
;;

let _rename_tyvars_in_struct_item
    (ctx : rename_context) (item : Ast.struct_item)
  : (rename_context * Ast.struct_item) =
  match item.struct_item_desc with
  | Struct_eval expr -> 
    let (ctx, expr) = _rename_tyvars_in_expr ctx expr in
    let item = { item with struct_item_desc = Struct_eval expr } in
    (ctx, item)

  | Struct_bind (rec_flag, bds) ->
    let ctx = _ctx_open_scope ctx in
    let ctx, bds = _rename_tyvars_in_let_bindings ctx bds in
    let ctx = _ctx_close_scope ctx in
    let item = { item with struct_item_desc = Struct_bind (rec_flag, bds) } in
    (ctx, item)
;;

let rename_tyvars_in_ast_struct t structure =
  let ctx = _init_ctx t in
  let ctx, rev_struct_items = List.fold_left
      (fun (ctx, rev_struct_items) item ->
         let ctx, item = _rename_tyvars_in_struct_item ctx item in
         (ctx, item::rev_struct_items))
      (ctx, [])
      structure
  in
  (ctx.namer, List.rev rev_struct_items)
;;
