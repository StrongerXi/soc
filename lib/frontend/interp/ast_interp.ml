open Pervasives

type native_func =
  | Builtin_println
  | Builtin_equal

(** Result of evaluating an AST expression *)
type value =
  | Int of int
  | Bool of bool
  | Closure of string list * Ast.expression * (context ref)
               (* arg names, body expr, enclosing context *)

and ident_desc =
  | Value of value
  | Native of native_func
  | Primop of Primops.op_kind
                
(** A [context] is an immutable execution context for the interpreter *)
and context = (string, ident_desc) Map.t

let _int_type = "int"
and _bool_type = "bool"
and _function_type = "function"
;;

let _println_val (v : value) : unit =
  match v with
  | Int n      -> Io.println (Int.to_string n)
  | Bool true  -> Io.println "true"
  | Bool false -> Io.println "false"
  | Closure _  -> Io.println "<function>"
;;

let _type_of (v : value) : string = (* TODO better type representation *)
  match v with
  | Int _     -> _int_type
  | Bool _    -> _bool_type
  | Closure _ -> _function_type
;;

let _context_lookup (ctx : context) (name : string) : ident_desc option =
  Map.get name ctx
;;

let _context_insert (ctx : context) (name : string) (desc : ident_desc) : context =
  Map.add name desc ctx
;;

let _context_insert_pairs (ctx : context) (pairs : (string * value) list)
  : context =
  List.fold_left (fun ctx (n, v) -> _context_insert ctx n (Value v)) ctx pairs
;;


exception Ast_interp_error of Errors.ast_interp_error
let _error (err : Errors.ast_interp_error) : 'a =
  raise (Ast_interp_error err)
;;

let _error_unbound_var (name : string) (span : Span.t) : 'a =
  _error (Errors.Ast_interp_unbound_var (name, span))
;;

let _error_type_mismatch (expect : string) (actual : string) (span : Span.t)
  : 'a =
  _error (Errors.Ast_interp_type_mismatch (expect, actual, span))
;;

let _error_arity_mismatch (expect : int) (actual : int) (span : Span.t) : 'a =
  _error (Errors.Ast_interp_arity_mismatch (expect, actual, span))
;;

let _error_letrec_invalid_rhs (span : Span.t) : 'a =
  _error (Errors.Ast_interp_letrec_invalid_rhs span)
;;


type _short_circuit = (* for evaluating boolean ops *)
  | SC_on_true
  | SC_on_false

let _interp_const (const : Ast.constant) : value =
  match const with
  | Const_Int n -> Int n
  | Const_Bool b -> Bool b
;;

let rec _interp_expr (ctx : context) (expr : Ast.expression) : value =
  match expr.expr_desc with
  | Exp_const const -> _interp_const const
  | Exp_ident name  -> _interp_name ctx name expr.expr_span
  | Exp_fun (arg_names, body) ->
    let names =
      List.map (fun (x : Ast.opt_typed_var) -> x.var) arg_names in
    Closure (names, body, ref ctx)
  | Exp_if (cnd, thn, els)        -> _interp_if_expr    ctx cnd thn els
  | Exp_let (rec_flag, bds, body) -> _interp_let_expr   ctx rec_flag bds body
  | Exp_apply (func, args)        -> _interp_apply      ctx func args expr.expr_span

and _interp_name (ctx : context) (name : string) (where : Span.t) : value =
  let make_closure_for_primop (op_kind : Primops.op_kind) : value =
    match op_kind with
    | AddInt | SubInt | MulInt | LogicAnd | LogicOr | LtInt ->
      _partial_apply ctx (Primop op_kind) [] 2
  in
  let make_closure_for_native (func : native_func) : value =
    match func with
    | Builtin_println -> _partial_apply ctx (Native func) [] 1
    | Builtin_equal -> _partial_apply ctx (Native func) [] 2
  in
  match _context_lookup ctx name with
  | None -> _error_unbound_var name where
  | Some (Value v) -> v
  | Some (Primop op_kind) -> make_closure_for_primop op_kind
  | Some (Native func) -> make_closure_for_native func

and _interp_check_int (ctx :context) (expr : Ast.expression) : int =
  match _interp_expr ctx expr with
  | Int n -> n
  | _ as v -> _error_type_mismatch _int_type (_type_of v) expr.expr_span

and _interp_boolean_binop (ctx : context)
    (lhs : Ast.expression) (rhs : Ast.expression) (sc : _short_circuit)
  : value =
  let lhs_v = _interp_expr ctx lhs in
  match lhs_v, sc with
  | Bool true, SC_on_true -> lhs_v
  | Bool false, SC_on_false -> lhs_v
  | Bool _, _ ->
    begin
      match _interp_expr ctx rhs with
      | Bool b -> Bool b
      | _ as v -> _error_type_mismatch _bool_type (_type_of v) rhs.expr_span
    end
  | _ -> _error_type_mismatch _bool_type (_type_of lhs_v) lhs.expr_span

and _interp_if_expr (ctx : context)
    (cnd : Ast.expression) (thn : Ast.expression) (els : Ast.expression)
  : value =
  match _interp_expr ctx cnd with
  | Bool true -> _interp_expr ctx thn
  | Bool false -> _interp_expr ctx els
  | _ as v -> _error_type_mismatch _bool_type (_type_of v) cnd.expr_span

and _interp_let_expr (ctx : context)
    (rec_flag : Ast.rec_flag) (bds : Ast.binding list) (body : Ast.expression)
  : value =
  let ctx = _interp_let_bindings ctx rec_flag bds in
  _interp_expr ctx body

and _interp_let_bindings (ctx : context)
    (rec_flag : Ast.rec_flag) (bds : Ast.binding list) : context =
  let _interp_one_binding (b : Ast.binding) : (string * value) =
    let name = b.binding_lhs.var in
    match b.binding_rhs.expr_desc, rec_flag with (* value restriction *)
    | _, Nonrecursive | Exp_const _, _ | Exp_fun _, _ ->
      let rhs_v = _interp_expr ctx b.binding_rhs in (name, rhs_v)
    | _ -> _error_letrec_invalid_rhs b.binding_rhs.expr_span
  in
  let name_arg_pairs = List.map _interp_one_binding bds in
  let ctx = _context_insert_pairs ctx name_arg_pairs in
  (match rec_flag with
  | Recursive ->
    List.iter (fun (_, v) ->
        match v with
        | Closure (_, _, ctx_ref) -> ctx_ref := ctx
        | _ -> ())
      name_arg_pairs
  | Nonrecursive -> ());
  ctx

(* - handle primitive op and native functions
 * - handle partial application *)
and _interp_apply (ctx : context)
    (func : Ast.expression) (args : Ast.expression list) (span : Span.t)
  : value =
    (* inline application for non-value func, or get stuck *)
  match func with
  | { expr_desc = Exp_ident name; expr_span; expr_typ = None } ->
    _interp_ident_apply ctx name expr_span args span
  | _ ->
    let func_value = _interp_expr ctx func in
    _interp_apply_func_value ctx func_value func.expr_span args span

and _interp_ident_apply (ctx : context)
    (func_ident : string) (func_span : Span.t) (args : Ast.expression list)
    (apply_span : Span.t)
  : value =
  (* NOTE this should synch with _intern_ident (not exactly tho) *)
  match _context_lookup ctx func_ident with
  | None -> _error_unbound_var func_ident func_span
  | Some (Value v) -> _interp_apply_func_value ctx v func_span args apply_span
  | Some (Primop op_kind) -> _interp_primop ctx op_kind func_span args apply_span
  | Some (Native func) -> _interp_native_apply ctx func func_span args apply_span

and _interp_primop (ctx : context)
    (op_kind : Primops.op_kind) (op_span : Span.t) (args : Ast.expression list)
    (apply_span : Span.t)
  : value =
  match args with
  | lhs::rhs::more_args ->
    let op_res = _interp_binary_primop ctx op_kind lhs rhs in
    let func_span = Span.merge op_span rhs.expr_span in
    _apply_if_any_args ctx op_res func_span more_args apply_span
  | _ -> _partial_apply ctx (Primop op_kind) args 2

and _interp_binary_primop (ctx : context)
    (primop : Primops.op_kind) (lhs : Ast.expression) (rhs : Ast.expression)
  : value =
  match primop with
  | LogicAnd -> _interp_boolean_binop ctx lhs rhs SC_on_false
  | LogicOr  -> _interp_boolean_binop ctx lhs rhs SC_on_true
  | _ ->
    let n1 = _interp_check_int ctx lhs in
    let n2 = _interp_check_int ctx rhs in
    match primop with
    | AddInt -> Int (n1 + n2)
    | SubInt -> Int (n1 - n2)
    | MulInt -> Int (n1 * n2)
    | LtInt  -> Bool (n1 < n2)
    (* TODO, why does compiler complain w/o this case? *)
    | _ -> failwith "[Ast_interp] unreachablle match case"

and _interp_apply_func_value (ctx : context)
    (func_val : value) (func_span : Span.t) (args : Ast.expression list)
    (apply_span : Span.t) : value =
  let _bind_args (names : string list) (args : Ast.expression list)
    : (((string * Ast.expression) list) * Span.t * Ast.expression list) option =
    (* Some (name_arg_pairs, span of last arg expr, more arg exprs);
     * Error --> missing >= 1 args *)
    let rec go names args
        (name_arg_pairs : (string * Ast.expression) list)
        (last_arg_expr : Span.t) =
      match names, args with
      | [], args -> Some(name_arg_pairs, last_arg_expr, args)
      | _, [] -> None
      | name::more_names, arg::more_args ->
        go more_names more_args ((name, arg)::name_arg_pairs) arg.expr_span
    in
    match names, args with
    | name::more_names, arg::more_args ->
      go more_names more_args [(name, arg)] arg.expr_span
    | _ -> failwith "[Ast_interp._internp_apply_func_value] expect >= 1 args"
  in
  match func_val with
  | Closure (names, body, func_ctx) ->
    begin
      match _bind_args names args with
      | Some (name_arg_pairs, last_arg_span, more_args) ->
        let name_arg_val_pairs =
          List.map (fun (name, arg) -> (name, _interp_expr ctx arg))
            name_arg_pairs
        in
        let ctx = _context_insert_pairs !func_ctx name_arg_val_pairs in
        let apply_res = _interp_expr ctx body in
        let res_span = Span.merge func_span last_arg_span in
        _apply_if_any_args ctx apply_res res_span more_args apply_span
      | None ->
        let expected_args_num = List.length names in
        _partial_apply ctx (Value func_val) args expected_args_num
    end
  | _ -> _error_type_mismatch _function_type (_type_of func_val) func_span

and _interp_native_apply (ctx : context)
    (func : native_func) (func_span : Span.t) (args : Ast.expression list)
    (apply_span : Span.t) : value =
  match func with
  | Builtin_equal ->
    begin
      match args with
      | lhs_e::rhs_e::more_args ->
        let lhs_v = _interp_expr ctx lhs_e in
        let rhs_v = _interp_expr ctx rhs_e in
        let func_span = Span.merge func_span rhs_e.expr_span in
        let result_v = Bool (lhs_v = rhs_v) in
        _apply_if_any_args ctx result_v func_span more_args apply_span
      | _ -> _partial_apply ctx (Native func) args 2
    end

  | Builtin_println ->
    match args with
    | arg_e::more_args ->
      let arg_v = _interp_expr ctx arg_e in
      let func_span = Span.merge func_span arg_e.expr_span in
      _println_val arg_v;
      _apply_if_any_args ctx arg_v func_span more_args apply_span
    | _ -> _partial_apply ctx (Native func) args 1

(* TODO unsure about the semantics of such eval, I know order is
 * unspecified, but do extra args get evaled eagerly? Well typechecking ensures
 * no extra args... So I get to decide here:) *)
and _apply_if_any_args (ctx :context)
    (func_val : value) (func_span : Span.t) (args : Ast.expression list)
    (apply_span : Span.t) : value =
  match args with
  | [] -> func_val
  | args -> _interp_apply_func_value ctx func_val func_span args apply_span

(* let f x y = x + y;;
 * f x 
 * ---->
 * let f = f
 * and x = x
 * in (fun y -> f x y) *)
and _partial_apply (original_ctx : context)
    (func_desc : ident_desc) (args : Ast.expression list)
    (expected_args_num : int) : value =
  let provided_args_num = List.length args in
  let extra_args_needed = expected_args_num - provided_args_num in
  if extra_args_needed <= 0
  then failwith "[Ast_interp._partial_apply] application already has enough args";
  let provided_arg_names =
    List.init provided_args_num (fun n -> "provided_" ^ (Int.to_string n))
  in
  let ctx_with_provided_args =
    List.fold_left (* evalaute left to right, my choice *)
      (fun ctx (name, arg_e) -> 
         _context_insert ctx name (Value (_interp_expr original_ctx arg_e)))
      original_ctx
      (List.combine provided_arg_names args)
  in
  let func_name = "f_partial" in
  let ctx_with_everything =
    _context_insert ctx_with_provided_args func_name func_desc in
  let extra_arg_names = 
    List.init extra_args_needed (fun n -> "extra_" ^ (Int.to_string n))
  in
  let make_dummy_expr (expr_desc : Ast.expr_desc) : Ast.expression =
    { Ast.expr_desc (* NOTE I don't think the span info will be used, ba. *)
    ; expr_span = Span.create (Location.create 0 0) (Location.create 0 0)
    ; expr_typ = None }
  in
  let func_ident_e = make_dummy_expr (Exp_ident func_name) in
  let arg_ident_es = List.map
      (fun arg_name -> make_dummy_expr (Exp_ident arg_name))
      (provided_arg_names @ extra_arg_names)
  in
  let body_e = make_dummy_expr (Exp_apply (func_ident_e, arg_ident_es)) in
  Closure(extra_arg_names, body_e, ref ctx_with_everything)
;;

let _interp_struct_item (ctx : context) (item : Ast.struct_item) : context =
  match item.struct_item_desc with
  | Struct_eval expr -> let _ = _interp_expr ctx expr in ctx
  | Struct_bind (rec_flag, bds) -> _interp_let_bindings ctx rec_flag bds
;;

let _interp_struct_impl items =
  let init_ctx = String.empty_map () in
  let init_ctx = Map.add "println" (Native Builtin_println) init_ctx in
  let init_ctx = Map.add "=" (Native Builtin_equal) init_ctx in
  let init_ctx =
    List.fold_left
      (fun ctx (info : Primops.op_info) ->
         Map.add info.opstr (Primop info.kind) ctx)
      init_ctx Primops.all_op_infos
  in
  let _ = List.fold_left _interp_struct_item init_ctx items in
  ()
;;

let interp_struct items =
  try Ok (_interp_struct_impl items)
  with Ast_interp_error err -> Error err
;;
