open Pervasives

(* the renamer *)
type t =
  { stamp      : int                    (* used to generate unique vars *)
  ; old_to_new : (string, string) Map.t (* old variable to new variable *)
  }

let _init_t =
  { stamp      = 0
  ; old_to_new = Map.empty String.compare
  }
;;

let _get_new_name_or_stay t (old_name : string) : (t * string) =
  match Map.get old_name t.old_to_new with
  | None          -> (t, old_name)
  | Some new_name -> (t, new_name)
;;

let _gen_and_add_new_name t (old_name : string) : (t * string) =
  let new_name = String.append old_name (Int.to_string t.stamp) in
  let t = { stamp      = t.stamp + 1
          ; old_to_new = Map.add old_name new_name t.old_to_new } in
  (t, new_name)
;;


let _rename_opt_typed_var t (otv : Ast.opt_typed_var)
    : (t * Ast.opt_typed_var) =
  let t, new_name = _gen_and_add_new_name t otv.var.stuff in
  let var = { otv.var with stuff = new_name } in
  (t, { otv with var })
;;

let rec _rename_opt_typed_vars t (otvs : Ast.opt_typed_var list)
    : (t * Ast.opt_typed_var list) =
  match otvs with
  | [] -> (t, [])
  | otv::rest ->
    let t, renamed_otv = _rename_opt_typed_var t otv in
    let t, renamed_rest = _rename_opt_typed_vars t rest in
    (t, renamed_otv::renamed_rest)
;;

let rec _rename_expr t (expr : Ast.expression) : (t * Ast.expression) =
  match expr.expr_desc with
  | Exp_const _ -> (t, expr)

  | Exp_ident name -> (* unbound name is untouched *)
    let t, new_name = _get_new_name_or_stay t name in
    let expr = { expr with expr_desc = Exp_ident new_name } in
    (t, expr)

  | Exp_binop (binop, lhs, rhs) ->
    _rename_binop_expr t expr.expr_span binop lhs rhs

  | Exp_if (cnd, thn, els) ->
    _rename_if_expr t expr.expr_span cnd thn els
      
  | Exp_apply (func, args) ->
    _rename_apply_expr t expr.expr_span func args

  | Exp_fun (params, body) ->
    _rename_fun_expr t expr.expr_span params body

  | Exp_let (rec_flag, bds, body) ->
    _rename_let_expr t expr.expr_span rec_flag bds body

and _rename_binop_expr t (expr_span : Span.t)
    (binop : Ast.binary_op) (lhs : Ast.expression) (rhs : Ast.expression)
  : (t * Ast.expression) =
  let t, lhs = _rename_expr t lhs in
  let t, rhs = _rename_expr t rhs in
  let expr = { Ast.expr_desc = Exp_binop (binop, lhs, rhs); expr_span } in
  (t, expr)

and _rename_if_expr t (expr_span : Span.t)
    (cnd : Ast.expression) (thn : Ast.expression) (els : Ast.expression)
  : (t * Ast.expression) =
  let t, cnd = _rename_expr t cnd in
  let t, thn = _rename_expr t thn in
  let t, els = _rename_expr t els in
  let expr = { Ast.expr_desc = Exp_if (cnd, thn, els); expr_span } in
  (t, expr)

and _rename_apply_expr t (expr_span : Span.t)
    (func : Ast.expression) (args : Ast.expression list)
  : (t * Ast.expression) =
  let t, func = _rename_expr t func in
  let t, rev_args = List.fold_left
      (fun (t, rev_args) arg ->
         let t, arg = _rename_expr t arg in
         (t, arg::rev_args))
      (t, []) args
  in
  let args = List.rev rev_args in
  let expr = { Ast.expr_desc = Exp_apply (func, args); expr_span } in
  (t, expr)

and _rename_let_expr t (expr_span : Span.t)
    (rec_flag : Ast.rec_flag) (bds : Ast.binding list) (body : Ast.expression)
  : (t * Ast.expression) =
  let t, bds = _rename_let_bindings t rec_flag bds in
  let t, body = _rename_expr t body in
  let expr = { Ast.expr_desc = Exp_let (rec_flag, bds, body); expr_span } in
  (t, expr)

and _rename_let_bindings t
    (rec_flag : Ast.rec_flag) (bds : Ast.binding list)
  : (t * Ast.binding list) =
  let rec _rename_nonrec t (bds : Ast.binding list) =
    match bds with
    | [] -> (t, [])
    | bd::rest -> (* must rename rhs first since lhs isn't in scope *)
      let t, renamed_rhs = _rename_expr t bd.binding_rhs in
      let t, renamed_lhs = _rename_opt_typed_var t bd.binding_lhs in
      let renamed_bd = { Ast.binding_lhs = renamed_lhs
                       ; binding_rhs = renamed_rhs } in
      let t, renamed_rest = _rename_nonrec t rest in
      (t, renamed_bd::renamed_rest)
  in
  let _rename_rec t (bds : Ast.binding list) =
    (* rename all lhs first *)
    let t, rev_renamed_lhss, rev_rhss = List.fold_left
      (fun (t, rev_renamed_lhss, rev_rhss) (bd : Ast.binding) ->
        let t, renamed_lhs = _rename_opt_typed_var t bd.binding_lhs in
        (t, renamed_lhs::rev_renamed_lhss, bd.binding_rhs::rev_rhss))
      (t, [], []) bds
    in
    let t, rev_bds = List.fold_left  (* then rename all rhs *)
        (fun (t, rev_bds) (renamed_lhs, rhs) ->
           let t, renamed_rhs = _rename_expr t rhs in
           let renamed_bd = { Ast.binding_lhs = renamed_lhs
                            ; binding_rhs = renamed_rhs } in
           (t, renamed_bd::rev_bds)
        )
        (t, []) (List.rev (List.combine rev_renamed_lhss rev_rhss))
    in
    (t, List.rev rev_bds)
  in
  match rec_flag with
  | Nonrecursive -> _rename_nonrec t bds
  | Recursive -> _rename_rec t bds

and _rename_fun_expr t (expr_span : Span.t)
    (params : Ast.opt_typed_var list) (body : Ast.expression)
  : (t * Ast.expression) =
  let t, params = _rename_opt_typed_vars t params in
  let t, body = _rename_expr t body in
  let expr = { Ast.expr_desc = Exp_fun (params, body); expr_span } in
  (t, expr)
;;

let _rename_struct_item t (item : Ast.struct_item) : (t * Ast.struct_item) =
  match item.struct_item_desc with
  | Struct_eval expr ->
    let (t, expr) = _rename_expr t expr in
    let item = { item with struct_item_desc = Struct_eval expr } in
    (t, item)

  | Struct_bind (rec_flag, bds) ->
    let (t, bds) = _rename_let_bindings t rec_flag bds in
    let item = { item with struct_item_desc = Struct_bind (rec_flag, bds) } in
    (t, item)
;;

let _rename_struct t (structure : Ast.structure) : (t * Ast.structure) =
  let t, rev_items = List.fold_left
      (fun (t, rev_items) item ->
         let t, item = _rename_struct_item t item in
         (t, item::rev_items))
      (t, []) structure
  in
  (t, List.rev rev_items)
;;

let rename_struct structure =
  let t = _init_t in
  let _, structure = _rename_struct t structure in
  structure
;;
