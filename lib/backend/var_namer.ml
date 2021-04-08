open Pervasives

type t =
  { stamp : int (* for generating unique names *)
  }

let init =
  { stamp = 0
  }
;;

let gen_new_var_with_prefix t pfx =
  let name = (String.append pfx (String.append "_" (Int.to_string t.stamp))) in
  let t = { stamp = t.stamp + 1 } in
  (t, name)
;;


(* NOTE [_rename_vars_in_X] renames all the _bound_ variables in [X]; 
 * it accumulates a map from old to new names during the process. *)
let _rename_vars_in_opt_typed_var t (var_map : (string, string) Map.t)
    (otv : Ast.opt_typed_var) : (t * (string, string) Map.t * Ast.opt_typed_var) =
  (* binding always makes a new variable *)
  let old_name = otv.var in
  let t, new_name = gen_new_var_with_prefix t old_name in
  let var_map = Map.add old_name new_name var_map in
  t, var_map, { otv with var = new_name }
;;

let rec _rename_vars_in_expr t (var_map : (string, string) Map.t)
    (expr : Ast.expression) : (t * Ast.expression) =
  match expr.expr_desc with
  | Exp_const _ -> (t, expr)
  | Exp_ident old_name -> 
    begin
      match Map.get old_name var_map with
      | None -> (t, expr)
      | Some new_name ->
        let expr = { expr with expr_desc = Exp_ident new_name } in
        (t, expr)
    end

  | Exp_fun (names, body) ->
    let t, var_map, rev_names = List.fold_left
        (fun (t, var_map, rev_names) otv ->
           let t, var_map, name = _rename_vars_in_opt_typed_var t var_map otv in
           (t, var_map, name::rev_names))
        (t, var_map, [])
        names
    in
    let names = List.rev rev_names in
    let t, body = _rename_vars_in_expr t var_map body in
    let expr = { expr with expr_desc = Exp_fun (names, body) } in
    (t, expr)

  | Exp_if (cnd, thn, els) ->
    let t, cnd = _rename_vars_in_expr t var_map cnd in
    let t, thn = _rename_vars_in_expr t var_map thn in
    let t, els = _rename_vars_in_expr t var_map els in
    let expr = { expr with expr_desc = Exp_if (cnd, thn, els) } in
    (t, expr)

  | Exp_let (rec_flag, bds, body) ->
    let t, var_map, bds = _rename_vars_in_let_bindings t var_map rec_flag bds in
    let t, body = _rename_vars_in_expr t var_map body in
    let expr = { expr with expr_desc = Exp_let (rec_flag, bds, body) } in
    (t, expr)

  | Exp_apply (func, args) -> 
    let t, func = _rename_vars_in_expr t var_map func in
    let t, rev_args = List.fold_left
        (fun (t, rev_args) arg ->
           let t, arg = _rename_vars_in_expr t var_map arg in
           (t, arg::rev_args))
        (t, [])
        args
    in
    let args = List.rev rev_args in
    let expr = { expr with expr_desc = Exp_apply (func, args) } in
    (t, expr)

and _rename_vars_in_let_bindings t (var_map : (string, string) Map.t)
    (rec_flag : Ast.rec_flag) (bds : Ast.binding list)
  : (t * (string, string) Map.t * Ast.binding list) =

  let _rename_vars_in_one_binding t (var_map : (string, string) Map.t)
     (bd : Ast.binding) : (t * (string, string) Map.t * Ast.binding) =
    let t, var_map, binding_lhs =
      match rec_flag with (* lhs of rec case is already renamed *)
      | Recursive -> t, var_map, bd.binding_lhs
      | Nonrecursive -> _rename_vars_in_opt_typed_var t var_map bd.binding_lhs
    in
    let t, binding_rhs = _rename_vars_in_expr t var_map bd.binding_rhs in
    let bd = { Ast.binding_lhs; binding_rhs } in
    (t, var_map, bd)
  in
  let t, var_map, bds =
    match rec_flag with
    | Nonrecursive -> t, var_map, bds
    | Recursive -> (* each rec lhs are bound in all rhss *)
      List.fold_right
        (fun (bd : Ast.binding) (t, var_map, bds) ->
           let t, var_map, binding_lhs =
             _rename_vars_in_opt_typed_var t var_map bd.binding_lhs in
           let bd = { bd with binding_lhs } in
           (t, var_map, bd::bds))
        bds (t, var_map, [])
  in
  let t, var_map, rev_bds = List.fold_left
      (fun (t, var_map, rev_bds) bd ->
         let t, var_map, bd = _rename_vars_in_one_binding t var_map bd in
         (t, var_map, bd::rev_bds))
      (t, var_map, [])
      bds
  in
  let bds = List.rev rev_bds in
  (t, var_map, bds)
;;

let _rename_vars_in_struct_item t (var_map : (string, string) Map.t)
    (item : Ast.struct_item)
  : (t * (string, string) Map.t * Ast.struct_item) =
  match item.struct_item_desc with
  | Struct_eval expr -> 
    let t, expr = _rename_vars_in_expr t var_map expr in
    let item = { item with struct_item_desc = Struct_eval expr } in
    (t, var_map, item)
  | Struct_bind (rec_flag, bds) ->
    let t, var_map, bds = _rename_vars_in_let_bindings t var_map rec_flag bds in
    let item = { item with struct_item_desc = Struct_bind (rec_flag, bds) } in
    (t, var_map, item)
;;

let rename_struct t structure =
  let var_map = Map.empty String.compare in
  let t, _, rev_struct_items = List.fold_left
      (fun (t, var_map, rev_struct_items) item ->
         let t, var_map, item = _rename_vars_in_struct_item t var_map item in
         (t, var_map, item::rev_struct_items))
      (t, var_map, [])
      structure
  in
  (t, List.rev rev_struct_items)
;;
