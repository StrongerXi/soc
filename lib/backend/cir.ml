open Pervasives

type constant =
  | CInt of int
  | CBool of bool

type expr =
  | Cconst of constant
  | Cident of string
  | Cmk_closure of mk_closure
  | Clet of (string * expr) list * expr
  | Cletrec of (string * letrec_rhs) list * expr
  | Cif of expr * expr * expr
  | Cprimop of Primops.op_kind * expr list
  | Capply of expr * expr list
  | Cnative_apply of string * expr list

and mk_closure =
  { cls_name  : string
  ; free_vars : string list
  }

and letrec_rhs =
  | Rhs_const of constant
  | Rhs_mkcls of mk_closure

type closure =
  { args : string list
  ; free_vars : string list
  ; body : expr
  }

type prog =
  { closures : (string, closure) Map.t
  ; expr     : expr
  }


(* Context for the translation process from AST to CIR.
 * I decided not to make a new module for this since it's pretty simplistic *)
type context =
  { closures : (string, closure) Map.t
  ; namer    : Var_namer.t
  }

let _init_ctx namer =
  { closures = Map.empty String.compare; namer }
;;

let _gen_new_var_name (ctx : context) (prefix : string) : (context * string) =
  let namer, new_name = Var_namer.gen_new_var_with_prefix ctx.namer prefix in
  ({ ctx with namer }, new_name)
;;

(* Apply [_gen_new_var_name ctx prefix] [count] # of times *)
let _gen_new_var_names (ctx : context) (prefix : string) (count : int)
  : (context * string list) =
  List.fold_right
    (fun _ (ctx, extra_arg_names) ->
       let ctx, arg_name = _gen_new_var_name ctx prefix in
       (ctx, arg_name::extra_arg_names))
    (List.init count (fun x -> x))
    (ctx, [])
;;

let _add_closure (ctx : context) (name : string) (cls : closure) : context =
  { ctx with closures = Map.add name cls ctx.closures }
;;

let _get_all_closures (ctx : context) : (string, closure) Map.t =
  ctx.closures
;;

(* NOTE We can optimize free_var compuation by caching free_vars of last
 * examined expression in context, if it becomes a performance bottleneck *)
let _free_vars_in_expr (expr : Ast.expression) : string Set.t =
  let rec go (free_vars : string Set.t) (expr : Ast.expression)
    : string Set.t =
    match expr.expr_desc with
    | Exp_const _ -> free_vars
    | Exp_ident name  -> Set.add name free_vars
    | Exp_fun (otvs, body) ->
      let names = List.map (fun (otv : Ast.opt_typed_var) -> otv.var) otvs in
      let body_fvs = go (Set.empty String.compare) body in
      let body_fvs = List.fold_right Set.remove names body_fvs in
      Set.union free_vars body_fvs

    | Exp_if (cnd, thn, els) ->
      let free_vars = go free_vars cnd in
      let free_vars = go free_vars thn in
      go free_vars els

    | Exp_let (_, bds, body) ->
      let names = List.map (fun (bd : Ast.binding) -> bd.binding_lhs.var) bds in
      let body_fvs = go (Set.empty String.compare) body in
      let body_fvs = List.fold_right Set.remove names body_fvs in
      Set.union free_vars body_fvs

    | Exp_apply (func, args) ->
      let free_vars = go free_vars func in
      List.fold_left go free_vars args
  in
  go (Set.empty String.compare) expr
;;

let rec _count_args_in_typ (typ : Ast.typ) : int =
  match typ with
  | Typ_arrow (_, out_typ) -> 1 + (_count_args_in_typ out_typ)
  | _ -> 0
;;

(* We always generate a new closure when we create a mk_closure *)
let _add_and_mk_closure (ctx : context)
    (args : string list) (free_vars : string list) (body : expr)
  : (context * mk_closure) =
  let ctx, cls_name = _gen_new_var_name ctx "closure" in
  let cls = { args; free_vars; body } in
  let ctx = _add_closure ctx cls_name cls in
  let mk_cls = { cls_name; free_vars } in
  (ctx, mk_cls)
;;

(* Translate AST expression to CIR expression *)
let rec _from_ast_expr (ctx : context) (expr : Ast.expression)
  : (context * expr) =
  match expr.expr_desc with
  | Exp_const const -> (ctx, Cconst (_from_ast_const const))
  | Exp_ident name  -> (ctx, Cident name)

  | Exp_fun (otvs, body) ->
    let ctx, c_mkcls = _from_ast_func ctx otvs body in
    (ctx, Cmk_closure c_mkcls)

  | Exp_if (cnd, thn, els) ->
    let ctx, c_cnd = _from_ast_expr ctx cnd in
    let ctx, c_thn = _from_ast_expr ctx thn in
    let ctx, c_els = _from_ast_expr ctx els in
    let c_if = Cif (c_cnd, c_thn, c_els) in
    (ctx, c_if)

  | Exp_let (rec_flag, bds, body) ->
    let ctx, c_body = _from_ast_expr ctx body in (* order shouldn't matter *)
    _from_ast_let_bindings ctx rec_flag bds c_body

  | Exp_apply (func, args) -> _from_ast_apply ctx func args

and _from_ast_const (const : Ast.constant) : constant =
  match const with
  | Const_Int n -> CInt n
  | Const_Bool b -> CBool b

and _from_ast_func (ctx : context)
    (otvs : Ast.opt_typed_var list) (body : Ast.expression)
  : (context * mk_closure) =
  let arg_names = List.map (fun (otv : Ast.opt_typed_var) -> otv.var) otvs in
  let ctx, cls_body = _from_ast_expr ctx body in
  let free_vars = _free_vars_in_expr body
                  |> List.fold_right Set.remove arg_names
                  |> Set.to_list in
  _add_and_mk_closure ctx arg_names free_vars cls_body

(* Use [c_body] as the body of translated let *)
and _from_ast_let_bindings (ctx : context) (rec_flag : Ast.rec_flag)
    (bds : Ast.binding list) (c_body : expr)
  : (context * expr) =

  let _from_one_let_binding (ctx : context) (bd : Ast.binding)
    : (context * (string * expr)) =
    let ctx, c_rhs = _from_ast_expr ctx bd.binding_rhs in
    let bd_name = bd.binding_lhs.var in
    (ctx, (bd_name, c_rhs))
  in
  let _from_one_letrec_binding (ctx : context) (bd : Ast.binding)
    : (context * (string * letrec_rhs)) =
    let bd_name = bd.binding_lhs.var in
    match bd.binding_rhs.expr_desc with
    | Exp_const const ->
      let c_const = _from_ast_const const in
      (ctx, (bd_name, Rhs_const c_const))

    | Exp_fun (otvs, body) ->
      let ctx, c_mkcls = _from_ast_func ctx otvs body in
      (ctx, (bd_name, Rhs_mkcls c_mkcls))

    | _ -> failwith "[Cir._from_ast_let_bindings] Illegal RHS of Letrec"
  in

  match rec_flag with
  | Nonrecursive -> (* simply translate each binding *)
    let ctx, c_bds = List.fold_right
        (fun bd (ctx, c_bds) ->
           let ctx, c_bd = _from_one_let_binding ctx bd in
           (ctx, c_bd::c_bds))
        bds (ctx, [])
    in
    let c_let = Clet (c_bds, c_body) in
    (ctx, c_let)

  | Recursive -> (* Could do some abstraction. Borderline case *)
    let ctx, c_bds = List.fold_right
        (fun bd (ctx, c_bds) ->
           let ctx, c_bd = _from_one_letrec_binding ctx bd in
           (ctx, c_bd::c_bds))
        bds (ctx, [])
    in
    let c_let = Cletrec (c_bds, c_body) in
    (ctx, c_let)

(* handle partial application too *)
and _from_ast_apply (ctx : context)
    (func : Ast.expression) (args : Ast.expression list)
  : (context * expr) =
  let expected_args_num = match func.expr_typ with
    | Some typ -> _count_args_in_typ typ
    | None ->
      failwith "[Cir._from_ast_apply] func expr should be annotated with type"
  in
  let extra_args_needed = expected_args_num - (List.length args) in
  if extra_args_needed < 0
  then failwith "[Cir._from_ast_apply] can't provide more args than expected";
  (* translate func expr and provided args *)
  let ctx, c_func = _from_ast_expr ctx func in
  let ctx, c_args =
    List.fold_right
      (fun arg (ctx, c_args) ->
         let ctx, c_arg = _from_ast_expr ctx arg in
         (ctx, c_arg::c_args))
      args (ctx, [])
  in
  if extra_args_needed = 0
  then (ctx, Capply (c_func, c_args))
  else (* extra_args_needed > 0 *)
    _partial_apply ctx c_func c_args extra_args_needed

(* (* ASSUME e1 takes 3 args *)
 * e1 e2
 * ----->
 * let f = e1
 * and x = e2
 * in (fun a1 a2 -> f x a1 a2) *)
and _partial_apply (ctx : context)
    (c_func : expr) (c_args : expr list) (extra_args_needed : int)
  : (context * expr) =
  let ctx, func_bind_name = _gen_new_var_name ctx "partial_applied_func" in
  let func_bind = (func_bind_name, c_func) in
  let ctx, provided_arg_bds =
    List.fold_right (* binding order matters for making Capply below *)
      (fun c_arg (ctx, provided_arg_bds) ->
         let ctx, arg_name = _gen_new_var_name ctx "provided_arg" in
         let provided_arg_bds = (arg_name, c_arg)::provided_arg_bds in
         (ctx, provided_arg_bds))
      c_args (ctx, [])
  in
  let ctx, extra_arg_names =
    _gen_new_var_names ctx "extra_arg" extra_args_needed
  in
  let provided_arg_names = List.map (fun (var, _) -> var) provided_arg_bds in
  let all_arg_names = List.append provided_arg_names extra_arg_names in
  let cls_body = Capply (Cident func_bind_name,
                         List.map (fun name -> Cident name) all_arg_names) in
  let ctx, mkcls =
    _add_and_mk_closure ctx extra_arg_names provided_arg_names cls_body in
  let let_binds = func_bind::provided_arg_bds in
  let c_let = Clet (let_binds, Cmk_closure mkcls) in
  (ctx, c_let)
;;


(* let rec x1 = e1;;
 * let x2 = e2;;
 * e3;;
 * e4;;
 * --->
 * let rec x1 = e1 in
 * let x2 = e2 in
 * let _ = e3 in
 * let _ = e4 in
 * body_e *)
let _from_ast_struct_item (ctx : context) (body_ce : expr)
    (item : Ast.struct_item) : (context * expr) =
  match item.struct_item_desc with
  | Struct_eval expr -> 
    let ctx, cexpr = _from_ast_expr ctx expr in
    let ctx, new_name = _gen_new_var_name ctx "struct_item" in
    let bds = [(new_name, cexpr)] in
    let let_cexpr = Clet (bds, body_ce) in
    (ctx, let_cexpr)

  | Struct_bind (rec_flag, bds) ->
    _from_ast_let_bindings ctx rec_flag bds body_ce
;;

(* Manually add all primitives/natives into the cir closure context and create
 * bindings for them. NOTE `external` syntax would move this to source *)
let _add_primops_closures (ctx : context) : (context * (string * expr) list) =

  let add_and_make_primop_closure (ctx : context) (info : Primops.op_info)
    : (context * mk_closure) =
    let arg_count = _count_args_in_typ info.typ in
    let ctx, arg_names = _gen_new_var_names ctx "prim_arg" arg_count in
    let free_vars = [] in
    let arg_idents = List.map (fun id -> Cident id) arg_names in
    let body = Cprimop (info.kind, arg_idents) in
    (* This is where we really need [ctx]: make unique name for closure *)
    _add_and_mk_closure ctx arg_names free_vars body
  in
  (* e.g.,
   * <closure> (addInt) x y = (AddInt, [x; y]) 
   * ...
   * let (+) = mkcls (addInt ...) in ... *)
  List.fold_right
    (fun (info : Primops.op_info) (ctx, bds) ->
       let ctx, mkcls = add_and_make_primop_closure ctx info in
       let bd = (info.opstr, Cmk_closure mkcls) in
       (ctx, bd::bds))
    Primops.all_op_infos (ctx, [])
;;

let _add_natives_closures (ctx : context) : (context * (string * expr) list) =
  (* name, label_str, arity *)
  let natives = [("=", "equal", 2)] in

  let add_and_make_native_closure
      (ctx : context) (label_str : string) (arity : int)
    : (context * mk_closure) =
    let ctx, args = _gen_new_var_names ctx "native_arg" arity in
    let free_vars = [] in
    let arg_idents = List.map (fun id -> Cident id) args in
    let body = Cnative_apply (label_str, arg_idents) in
    _add_and_mk_closure ctx args free_vars body
  in
  List.fold_right
    (fun (name, label_str, arity) (ctx, bds) ->
       let ctx, mkcls = add_and_make_native_closure ctx label_str arity in
       let bd = (name, Cmk_closure mkcls) in
       (ctx, bd::bds))
    natives (ctx, [])
;;

(* NOTE ASSUME the translation order is irrelevant.
 * This makes translation easier for the let bindings *)
let from_ast_struct structure =
  let namer = Var_namer.init in
  let namer, structure = Var_namer.rename_struct namer structure in
  let ctx = _init_ctx namer in
  let dummy_body = Cconst (CInt 42) in (* no effect, simplifies code *)
  let ctx, final_ce =
    List.fold_right
      (fun item (ctx, final_ce) ->
         _from_ast_struct_item ctx final_ce item)
      structure (ctx, dummy_body)
  in
  let ctx, primop_bds = _add_primops_closures ctx in
  let ctx, native_bds = _add_natives_closures ctx in
  let builtin_bds = List.append primop_bds native_bds in
  { closures = _get_all_closures ctx
  ; expr = Clet (builtin_bds, final_ce)
  }
;;
