open Pervasives

type t =
  { stamp : int (* for generating new type variables *)
  }

let init =
  { stamp = 0
  }
;;

let _gen_new_tyvar_name t : (t * string) =
  let name = String.append "X" (Int.to_string t.stamp) in
  let t = { stamp = t.stamp + 1 } in
  (t, name)
;;

let gen_new_tyvar t : (t * Ast.typ_desc) =
  let t, name = _gen_new_tyvar_name t in
  let tyvar = Ast.Typ_var (Some name) in
  (t, tyvar)
;;


let rec _rename_tyvars_in_typ_desc t (tv_map : (string, string) Map.t)
    (desc : Ast.typ_desc) : (t * (string, string) Map.t * Ast.typ_desc) =
  match desc with
  | Typ_const _ -> (t, tv_map, desc)
  | Typ_var None -> (* [_] becomes a unique new tyvar *)
    let t, desc = gen_new_tyvar t in
    (t, tv_map, desc)
  | Typ_var (Some tv) ->
    begin
      match Map.get tv tv_map with
      | None -> (* first occurrence introduces binding implicitly *)
        let t, new_tv = _gen_new_tyvar_name t in
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

let _rename_tyvars_in_struct_item t (item : Ast.struct_item)
  : (t * Ast.struct_item) =
  let tv_map = Map.empty String.compare in
  match item.struct_item_desc with
  | Struct_eval expr -> 
    let (t, expr) = _rename_tyvars_in_expr t tv_map expr in
    let item = { item with struct_item_desc = Struct_eval expr } in
    (t, item)
  | Struct_bind (rec_flag, bds) ->
    let (t, _, bds) = _rename_tyvars_in_let_bindings t tv_map bds in
    let item = { item with struct_item_desc = Struct_bind (rec_flag, bds) } in
    (t, item)
;;

let rename_struct t structure =
  let t, rev_struct_items = List.fold_left
      (fun (t, rev_struct_items) item ->
         let t, item = _rename_tyvars_in_struct_item t item in
         (t, item::rev_struct_items))
      (t, [])
      structure
  in
  (t, List.rev rev_struct_items)
;;
