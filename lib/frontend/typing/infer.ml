open Pervasives

(* A short-hand to simplify common return types *)
type 'a ret = (Infer_ctx.t * Ast.typ_desc * 'a)


let _is_legal_let_rhs (rec_flag : Ast.rec_flag) (rhs : Ast.expression) : bool =
  match rec_flag, rhs.expr_desc with
  | Nonrecursive, _ -> true
  | Recursive, (Exp_const _ | Exp_fun _) -> true
  | _ -> false
;;

let _get_annotated_typ_desc (otv : Ast.opt_typed_var) : Ast.typ_desc =
  match otv.typ with
  | None ->  
    let msg = "[Infer._get_annotated_typ_desc]" in
    let msg = String.append msg " all variables should have type annotations" in
    failwith msg
  | Some typ -> typ.typ_desc
;;

let _add_opt_typed_vars (ctx : Infer_ctx.t) (otvs : Ast.opt_typed_var list)
  : Infer_ctx.t =
  List.fold_left 
    (fun ctx (otv : Ast.opt_typed_var) ->
       let name = otv.var.stuff in
       let typ = _get_annotated_typ_desc otv in
       Infer_ctx.add_type ctx name typ)
    ctx otvs
;;


let _infer_const (const : Ast.constant) : Ast.typ_desc =
  match const with
  | Const_Int  _ -> Builtin_types.int_typ
  | Const_Bool _ -> Builtin_types.bool_typ
;;

let rec _infer_expr
    (ctx : Infer_ctx.t) (expr : Ast.expression) : Ast.expression ret =
  let expr_span = expr.expr_span in
  match expr.expr_desc with
  | Exp_const const ->
    let typ = _infer_const const in (ctx, typ, expr)

  | Exp_ident name ->
    let (ctx, typ) = Infer_ctx.get_type ctx name expr_span in
    (ctx, typ, expr)

  | Exp_binop (binop, lhs, rhs) ->
    _infer_binop_expr ctx expr_span binop lhs rhs

  | Exp_if (cnd, thn, els) ->
    _infer_if_expr ctx expr_span cnd thn els

  | Exp_let (rec_flag, bds, body) ->
    _infer_let_expr ctx expr_span rec_flag bds body

  | Exp_fun (params, body) ->
    _infer_fun_expr ctx expr_span params body
      
  | Exp_apply (func, args) ->
    _infer_apply_expr ctx expr_span func args

and _infer_binop_expr
    (ctx : Infer_ctx.t) (expr_span : Span.t) (binop : Ast.binary_op)
    (lhs : Ast.expression) (rhs : Ast.expression) : Ast.expression ret =
  let (ctx, lhs_typ, lhs) = _infer_expr ctx lhs in
  let (ctx, rhs_typ, rhs) = _infer_expr ctx rhs in
  let (lhs_span, rhs_span) = (lhs.expr_span, rhs.expr_span) in
  let (ctx, out_ty) =
    Infer_ctx.unify_binop ctx binop lhs_typ lhs_span rhs_typ rhs_span in
  let expr = { Ast.expr_desc = Exp_binop (binop, lhs, rhs); expr_span } in
  (ctx, out_ty, expr)

and _infer_if_expr
    (ctx : Infer_ctx.t) (expr_span : Span.t) (cnd : Ast.expression)
    (thn : Ast.expression) (els : Ast.expression) : Ast.expression ret =
  let (ctx, cnd_typ, cnd) = _infer_expr ctx cnd in
  let ctx, _ =
    Infer_ctx.unify ctx Builtin_types.bool_typ cnd_typ cnd.expr_span in
  let (ctx, thn_typ, thn) = _infer_expr ctx thn in
  let (ctx, els_typ, els) = _infer_expr ctx els in
  let ctx, typ = Infer_ctx.unify ctx thn_typ els_typ els.expr_span in
  let expr = { Ast.expr_desc = Exp_if (cnd, thn, els); expr_span } in
  (ctx, typ, expr)

and _infer_let_expr 
    (ctx : Infer_ctx.t) (expr_span : Span.t)
    (rec_flag : Ast.rec_flag) (bds : Ast.binding list) (body : Ast.expression)
  : Ast.expression ret =
  let ctx = Infer_ctx.open_scope ctx in
  let (ctx, bds) = _infer_let_bindings ctx rec_flag bds in
  let (ctx, body_typ, body) = _infer_expr ctx body in
  let expr = { Ast.expr_desc = Exp_let (rec_flag, bds, body); expr_span } in
  let ctx = Infer_ctx.close_scope ctx in
  (ctx, body_typ, expr)

and _infer_let_bindings
    (ctx : Infer_ctx.t) (rec_flag : Ast.rec_flag) (bds : Ast.binding list)
  : (Infer_ctx.t * Ast.binding list) =
  (* can't put lhs of nonrec let into ctx yet, so we return the inferred rhs
   * type and unify it with lhs annotation later, to make rhs_typ up-to-date *)
  let _infer_one_binding
      (ctx : Infer_ctx.t) (bd : Ast.binding)
    : (Infer_ctx.t * (Ast.binding * Ast.typ_desc)) =
    let ctx =
      if _is_legal_let_rhs rec_flag bd.binding_rhs then ctx
      else Infer_ctx.add_error ctx
          (Errors.Infer_illegal_letrec_rhs bd.binding_rhs.expr_span)
    in 
    let (ctx, rhs_typ, rhs) = _infer_expr ctx bd.binding_rhs in
    let ctx = match rec_flag with
      | Nonrecursive -> ctx
      | Recursive ->
        let name, span = bd.binding_lhs.var.stuff, bd.binding_lhs.var.span in
        let ctx, lhs_typ = Infer_ctx.get_type ctx name span in
        let ctx, _ = Infer_ctx.unify ctx lhs_typ rhs_typ rhs.expr_span in
        ctx
    in
    (ctx, ({ bd with binding_rhs = rhs }, rhs_typ))
  in
  let ctx = match rec_flag with
    | Nonrecursive -> ctx
    | Recursive -> (* only let rec can access lhs in rhs *)
      let otvs = List.map (fun (bd : Ast.binding) -> bd.binding_lhs) bds in
      _add_opt_typed_vars ctx otvs
  in
  let (ctx, rev_bd_typ_pairs) = (* infer each binding *)
    List.fold_left
      (fun (ctx, rev_bd_typ_pairs) bd ->
         let (ctx, bd_typ) = _infer_one_binding ctx bd in
         (ctx, bd_typ::rev_bd_typ_pairs))
      (ctx, []) bds
  in
  (* generalize each binding and add it to ctx (overwrite for rec) *)
  let (ctx, rev_bds) =
    List.fold_right
      (fun ((bd : Ast.binding), (infer_typ : Ast.typ_desc)) (ctx, rev_bds) ->
         let name = bd.binding_lhs.var.stuff in
         match rec_flag with
         | Recursive ->
           let ctx = Infer_ctx.generalize ctx name in
           (* TODO an update binding abstraction, for here and [_infer_fun]
            * update letrec at the end, since later generalization *)
           let span = bd.binding_lhs.var.span in
           let ctx, rhs_desc = Infer_ctx.get_type ctx name span in
           let rhs_typ = { Ast.typ_desc = rhs_desc; typ_span = Span.dummy } in
           let new_lhs = { bd.binding_lhs with typ = Some rhs_typ } in
           let bd = { bd with binding_lhs = new_lhs } in
           (ctx, bd::rev_bds)
         | Nonrecursive -> 
           let rhs_span = bd.binding_rhs.expr_span in
           let annot_typ = _get_annotated_typ_desc bd.binding_lhs in
           let ctx, desc = Infer_ctx.unify ctx annot_typ infer_typ rhs_span in
           let typ = { Ast.typ_desc = desc; typ_span = Span.dummy } in
           let new_lhs = { bd.binding_lhs with typ = Some typ } in
           let bd = { bd with binding_lhs = new_lhs } in
           let ctx = Infer_ctx.add_type ctx name desc in
           let ctx = Infer_ctx.generalize ctx name in
           (ctx, bd::rev_bds))
        rev_bd_typ_pairs (ctx, [])
  in
  (ctx, List.rev rev_bds)

and _infer_fun_expr
    (ctx : Infer_ctx.t) (expr_span : Span.t)
    (params : Ast.opt_typed_var list) (body : Ast.expression)
  : Ast.expression ret =
  let ctx = Infer_ctx.open_scope ctx in
  let ctx = _add_opt_typed_vars ctx params in
  let (ctx, body_typ, body) = _infer_expr ctx body in
  let (ctx, params, ret_typ) = List.fold_right
      (fun (otv : Ast.opt_typed_var) (ctx, params, ret_typ) ->
         let name, span = otv.var.stuff, otv.var.span in
         let ctx, in_typ_desc = Infer_ctx.get_type ctx name span in
         let in_typ = match otv.typ with
           | None -> { Ast.typ_desc = in_typ_desc; typ_span = Span.dummy }
           | Some in_typ -> { in_typ with typ_desc = in_typ_desc }
         in
         let otv = { otv with typ = Some in_typ } in
         let ret_typ = Ast.Typ_arrow (in_typ_desc, ret_typ) in
         (ctx, otv::params, ret_typ))
      params (ctx, [], body_typ)
  in
  let ctx = Infer_ctx.close_scope ctx in
  let expr = { Ast.expr_desc = Exp_fun (params, body); expr_span } in
  (ctx, ret_typ, expr)

and _infer_apply_expr
    (ctx : Infer_ctx.t) (expr_span : Span.t)
    (func : Ast.expression) (args : Ast.expression list) : Ast.expression ret =
  let (ctx, func_typ, func) = _infer_expr ctx func in
  let (ctx, rev_arg_typs, rev_args) =
    List.fold_left
      (fun (ctx, rev_arg_typs, rev_args) arg ->
         let (ctx, arg_typ, arg) = _infer_expr ctx arg in
         (ctx, arg_typ::rev_arg_typs, arg::rev_args))
      (ctx, [], []) args
  in
  let arg_typs, args = List.rev rev_arg_typs, List.rev rev_args in
  let arg_spans = List.map (fun (arg : Ast.expression) -> arg.expr_span) args in
  let arg_typ_span_pairs = List.combine arg_typs arg_spans in
  let ctx, ret_typ =
    Infer_ctx.unify_apply ctx func_typ func.expr_span arg_typ_span_pairs in
  let expr = { Ast.expr_desc = Exp_apply (func, args); expr_span } in
  (ctx, ret_typ, expr)
;;

let _infer_struct_item
    (ctx : Infer_ctx.t) (item : Ast.struct_item)
  : (Infer_ctx.t * Ast.struct_item) =
  match item.struct_item_desc with
  | Struct_eval expr ->
    let (ctx, _, expr) = _infer_expr ctx expr in
    let item = { item with struct_item_desc = Struct_eval expr } in
    (ctx, item)
  | Struct_bind (rec_flag, bds) ->
    let (ctx, bds) = _infer_let_bindings ctx rec_flag bds in
    let item = { item with struct_item_desc = Struct_bind (rec_flag, bds) } in
    (ctx, item)
;;

let rec _infer_struct
    (ctx : Infer_ctx.t) (structure : Ast.structure)
  : (Infer_ctx.t * Ast.structure) =
  match structure with
  | [] -> (ctx, [])
  | item::structure ->
    let (ctx, item) = _infer_struct_item ctx item in
    let (ctx, items) = _infer_struct ctx structure in
    (ctx, item::items)
;;


let infer_struct structure =
  let ctx = Infer_ctx.empty in
  let (ctx, structure) = Infer_ctx.rename_tyvars ctx structure in
  let (ctx, structure) = _infer_struct ctx structure in
  match Infer_ctx.get_errors ctx with
  | [] -> Ok structure
  | errs -> Error errs
;;