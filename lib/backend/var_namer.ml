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


(* NOTE [_rename_vars_in_X] 
 * - renames all the _bound_ variables in [X]
 * - accumulates a map from old to new names, where old names are all variables
 *   currently in scope *)
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

and _rename_vars_in_let_bindings t
    (init_var_map : (string, string) Map.t)
    (rec_flag : Ast.rec_flag) (init_bds : Ast.binding list)
  : (t * (string, string) Map.t * Ast.binding list) =

  let rename_all_bd_lhs t var_map bds 
    : (t * (string, string) Map.t * Ast.binding list) =
    List.fold_right
      (fun (bd : Ast.binding) (t, var_map, bds) ->
         let t, var_map, binding_lhs =
           _rename_vars_in_opt_typed_var t var_map bd.binding_lhs in
         let bd = { bd with binding_lhs } in
         (t, var_map, bd::bds))
      bds (t, var_map, [])
  in

  let rename_all_bd_rhs t var_map bds
    : (t *  Ast.binding list) =
    List.fold_right
      (fun (bd : Ast.binding) (t, bds) ->
         let t, binding_rhs = _rename_vars_in_expr t var_map bd.binding_rhs in
         let bd = { bd with binding_rhs } in
         (t, bd::bds))
      bds (t, [])
  in

  match rec_flag with (* based on the scoping rules of [let] vs [let rec] *)
  | Nonrecursive ->
    let t, bds = rename_all_bd_rhs t init_var_map init_bds in
    rename_all_bd_lhs t init_var_map bds

  | Recursive ->
    let t, var_map_with_lhs, bds = rename_all_bd_lhs t init_var_map init_bds in
    let t, bds = rename_all_bd_rhs t var_map_with_lhs bds in
    (t, var_map_with_lhs, bds)
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
