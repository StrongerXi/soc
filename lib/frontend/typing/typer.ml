open Pervasives

(* A short-hand to simplify common return types *)
type 'a ret = (Typer_ctx.t * Ast.typ * 'a)


let _get_annotated_typ (otv : Ast.opt_typed_var) : Ast.typ =
  match otv.typ with
  | None ->  
    let msg = "[Typer._get_annotated_typ]" in
    let msg = String.append msg " all variables should have type annotations" in
    failwith msg
  | Some typ -> typ
;;


(* The following group of functions update all type annotations using [ctx] *)
let _update_tyvars_in_otv (ctx : Typer_ctx.t) (otv : Ast.opt_typed_var)
  : Ast.opt_typed_var =
  let typ = _get_annotated_typ otv in
  let typ = Typer_ctx.update_typ ctx typ in
  { otv with typ = Some typ }
;;

let rec _update_tyvars_in_expression (ctx : Typer_ctx.t) (expr : Ast.expression)
  : Ast.expression =
  let expr_desc = 
    match expr.expr_desc with
    | Exp_const _ | Exp_ident _ -> expr.expr_desc

    | Exp_if (cnd, thn, els) ->
      let cnd = _update_tyvars_in_expression ctx cnd in
      let thn = _update_tyvars_in_expression ctx thn in
      let els = _update_tyvars_in_expression ctx els in
      Exp_if (cnd, thn, els)

    | Exp_apply (func, args) ->
      let func = _update_tyvars_in_expression ctx func in
      let args = List.map (_update_tyvars_in_expression ctx) args in
      Exp_apply (func, args)

    | Exp_fun (params, body) ->
      let params = List.map (_update_tyvars_in_otv ctx) params in
      let body = _update_tyvars_in_expression ctx body in
      Exp_fun (params, body)

    | Exp_let (rec_flag, bds, body) ->
      let bds = List.map (_update_tyvars_in_let_binding ctx) bds in
      let body = _update_tyvars_in_expression ctx body in
      Exp_let (rec_flag, bds, body)
  in
  let expr_typ = Option.map (Typer_ctx.update_typ ctx) expr.expr_typ in
  { expr with expr_desc; expr_typ }

and _update_tyvars_in_let_binding (ctx : Typer_ctx.t) (binding : Ast.binding)
  : Ast.binding =
  let binding_lhs = _update_tyvars_in_otv ctx binding.binding_lhs in
  let binding_rhs = _update_tyvars_in_expression ctx binding.binding_rhs in
  { Ast.binding_lhs; binding_rhs }
;;

let _update_tyvars_in_struct_item (ctx : Typer_ctx.t) (item : Ast.struct_item)
  : Ast.struct_item =
  match item.struct_item_desc with
  | Struct_eval expr ->
    let expr = _update_tyvars_in_expression ctx expr in
    { item with struct_item_desc = Struct_eval expr }

  | Struct_bind (rec_flag, bds) ->
    let bds = List.map (_update_tyvars_in_let_binding ctx) bds in
    { item with struct_item_desc = Struct_bind (rec_flag, bds) }
;;

let _update_tyvars_in_struct (ctx : Typer_ctx.t) (structure : Ast.structure)
  : Ast.structure =
  List.map (_update_tyvars_in_struct_item ctx) structure
;;


let _is_legal_let_rhs (rec_flag : Ast.rec_flag) (rhs : Ast.expression) : bool =
  match rec_flag, rhs.expr_desc with
  | Nonrecursive, _ -> true
  | Recursive, (Exp_const _ | Exp_fun _) -> true
  | _ -> false
;;

let _add_opt_typed_vars (ctx : Typer_ctx.t) (otvs : Ast.opt_typed_var list)
  : Typer_ctx.t =
  List.fold_left 
    (fun ctx (otv : Ast.opt_typed_var) ->
       let name = otv.var in
       let typ = _get_annotated_typ otv in
       Typer_ctx.add_type ctx name typ)
    ctx otvs
;;


let _type_const (const : Ast.constant) : Ast.typ =
  match const with
  | Const_Int  _ -> Builtin_types.int_typ
  | Const_Bool _ -> Builtin_types.bool_typ
;;

(* If [expr] is annoated, unify [typ] with it and return result type.
 * Annotate [expr] with returned type *)
let _resolve_typ_annot
    (ctx : Typer_ctx.t) (expr : Ast.expression) (typ : Ast.typ)
  : Ast.expression ret =
  let ctx, typ = match expr.expr_typ with
    | None -> ctx, typ
    | Some annot -> Typer_ctx.unify ctx annot typ expr.expr_span
  in
  let expr = { expr with expr_typ = Some typ } in
  (ctx, typ, expr)
;;

let rec _type_expr
    (ctx : Typer_ctx.t) (expr : Ast.expression) : Ast.expression ret =
  let ctx, typ, expr = _type_expr_aux ctx expr in
  _resolve_typ_annot ctx expr typ

and _type_expr_aux (* type [expr] without touch ITS type annotation yet *)
    (ctx : Typer_ctx.t) (expr : Ast.expression) : Ast.expression ret =
  let expr_span = expr.expr_span in
  match expr.expr_desc with
  | Exp_const const ->
    let typ = _type_const const in
    (ctx, typ, expr)

  | Exp_ident name ->
    let (ctx, typ) = Typer_ctx.get_type ctx name expr_span in
    (ctx, typ, expr)

  | Exp_if (cnd, thn, els) ->
    _type_if_expr ctx expr_span cnd thn els

  | Exp_let (rec_flag, bds, body) ->
    _type_let_expr ctx expr_span rec_flag bds body

  | Exp_fun (params, body) ->
    _type_fun_expr ctx expr_span params body

  | Exp_apply (func, args) ->
    _type_apply_expr ctx expr_span func args

and _type_if_expr
    (ctx : Typer_ctx.t) (expr_span : Span.t) (cnd : Ast.expression)
    (thn : Ast.expression) (els : Ast.expression) : Ast.expression ret =
  let (ctx, cnd_typ, cnd) = _type_expr ctx cnd in
  let ctx, _ =
    Typer_ctx.unify ctx Builtin_types.bool_typ cnd_typ cnd.expr_span in
  let (ctx, thn_typ, thn) = _type_expr ctx thn in
  let (ctx, els_typ, els) = _type_expr ctx els in
  let ctx, typ = Typer_ctx.unify ctx thn_typ els_typ els.expr_span in
  let expr_typ = Some typ in
  let expr = { Ast.expr_desc = Exp_if (cnd, thn, els); expr_span; expr_typ } in
  (ctx, typ, expr)

and _type_let_expr 
    (ctx : Typer_ctx.t) (expr_span : Span.t)
    (rec_flag : Ast.rec_flag) (bds : Ast.binding list) (body : Ast.expression)
  : Ast.expression ret =
  let ctx = Typer_ctx.open_scope ctx in
  let (ctx, bds) = _type_let_bindings ctx rec_flag bds in
  let (ctx, body_typ, body) = _type_expr ctx body in
  let ctx = Typer_ctx.close_scope ctx in
  let expr_typ = Some body_typ in
  let expr =
    { Ast.expr_desc = Exp_let (rec_flag, bds, body); expr_span; expr_typ } in
  (ctx, body_typ, expr)

and _type_let_bindings
    (ctx : Typer_ctx.t) (rec_flag : Ast.rec_flag) (bds : Ast.binding list)
  : (Typer_ctx.t * Ast.binding list) =
  let _type_one_binding
      (ctx : Typer_ctx.t) (bd : Ast.binding) : (Typer_ctx.t * Ast.binding) =
    let ctx =
      if _is_legal_let_rhs rec_flag bd.binding_rhs then ctx
      else Typer_ctx.add_error ctx
          (Errors.Typer_illegal_letrec_rhs bd.binding_rhs.expr_span)
    in 
    let (ctx, rhs_typ, rhs) = _type_expr ctx bd.binding_rhs in
    let lhs_typ = _get_annotated_typ bd.binding_lhs in
    let ctx, _ = Typer_ctx.unify ctx lhs_typ rhs_typ rhs.expr_span in
    (ctx, { bd with binding_rhs = rhs })
  in
  let ctx = match rec_flag with
    | Nonrecursive -> ctx
    | Recursive -> (* let rec can access lhs in rhs *)
      let otvs = List.map (fun (bd : Ast.binding) -> bd.binding_lhs) bds in
      _add_opt_typed_vars ctx otvs
  in
  let (ctx, rev_bds) = (* type each binding *)
    List.fold_left
      (fun (ctx, rev_bds) bd ->
         let (ctx, bd) = _type_one_binding ctx bd in
         (ctx, bd::rev_bds))
      (ctx, []) bds
  in
  let ctx = match rec_flag with
    | Recursive -> ctx
    | Nonrecursive -> (* add lhs into context *)
      List.fold_left
        (fun ctx (bd : Ast.binding) ->
           let name = bd.binding_lhs.var in
           let lhs_typ = _get_annotated_typ bd.binding_lhs in
           (* [lhs_typ] will be promoted via substitutions *)
           Typer_ctx.add_type ctx name lhs_typ)
        ctx rev_bds
  in
  let names =
    List.map (fun (bd : Ast.binding) -> bd.binding_lhs.var) rev_bds in
  let ctx = Typer_ctx.generalize ctx names in
  (ctx, List.rev rev_bds)

and _type_fun_expr
    (ctx : Typer_ctx.t) (expr_span : Span.t)
    (params : Ast.opt_typed_var list) (body : Ast.expression)
  : Ast.expression ret =
  let ctx = Typer_ctx.open_scope ctx in
  let ctx = _add_opt_typed_vars ctx params in
  let (ctx, body_typ, body) = _type_expr ctx body in
  let (ctx, final_typ) = List.fold_right
      (fun (otv : Ast.opt_typed_var) (ctx, final_typ) ->
         let in_typ = _get_annotated_typ otv in
         let final_typ = Ast.Typ_arrow (in_typ, final_typ) in
         (ctx, final_typ))
      params (ctx, body_typ)
  in
  let ctx = Typer_ctx.close_scope ctx in
  let expr_typ = Some final_typ in
  let expr = { Ast.expr_desc = Exp_fun (params, body); expr_span; expr_typ } in
  (ctx, final_typ, expr)

and _type_apply_expr
    (ctx : Typer_ctx.t) (expr_span : Span.t)
    (func : Ast.expression) (args : Ast.expression list) : Ast.expression ret =
  let (ctx, func_typ, func) = _type_expr ctx func in
  let (ctx, rev_arg_typs, rev_args) =
    List.fold_left
      (fun (ctx, rev_arg_typs, rev_args) arg ->
         let (ctx, arg_typ, arg) = _type_expr ctx arg in
         (ctx, arg_typ::rev_arg_typs, arg::rev_args))
      (ctx, [], []) args
  in
  let arg_typs, args = List.rev rev_arg_typs, List.rev rev_args in
  let arg_spans = List.map (fun (arg : Ast.expression) -> arg.expr_span) args in
  let arg_typ_span_pairs = List.combine arg_typs arg_spans in
  let ctx, ret_typ =
    Typer_ctx.unify_apply ctx func_typ func.expr_span arg_typ_span_pairs in
  let expr_typ = Some ret_typ in
  let expr = { Ast.expr_desc = Exp_apply (func, args); expr_span; expr_typ } in
  (ctx, ret_typ, expr)
;;

let _type_struct_item
    (ctx : Typer_ctx.t) (item : Ast.struct_item)
  : (Typer_ctx.t * Ast.struct_item) =
  match item.struct_item_desc with
  | Struct_eval expr ->
    let (ctx, _, expr) = _type_expr ctx expr in
    let item = { item with struct_item_desc = Struct_eval expr } in
    (ctx, item)
  | Struct_bind (rec_flag, bds) ->
    let (ctx, bds) = _type_let_bindings ctx rec_flag bds in
    let item = { item with struct_item_desc = Struct_bind (rec_flag, bds) } in
    (ctx, item)
;;

let rec _type_struct
    (ctx : Typer_ctx.t) (structure : Ast.structure)
  : (Typer_ctx.t * Ast.structure) =
  match structure with
  | [] -> (ctx, [])
  | item::structure ->
    let (ctx, item) = _type_struct_item ctx item in
    let (ctx, items) = _type_struct ctx structure in
    (ctx, item::items)
;;


let type_struct strc =
  let (tv_namer, strc) = Tyvar_namer.rename_struct Tyvar_namer.init strc in
  let ctx = Typer_ctx.create tv_namer in
  let (ctx, strc) = _type_struct ctx strc in
  let strc = _update_tyvars_in_struct ctx strc in
  match Typer_ctx.get_errors ctx with
  | [] -> Ok strc
  | errs -> Error errs
;;
