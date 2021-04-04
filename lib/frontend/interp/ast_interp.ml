open Pervasives

type builtin_func =
  | Builtin_println

(** Result of evaluating an AST expression *)
type value =
  | Int of int
  | Bool of bool
  | Native of builtin_func
  | Closure of string list * Ast.expression * (context ref)
               (* arg names, body expr, enclosing context *)

(** A [context] is an immutable execution context for the interpreter *)
and context = (string, value) Map.t

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
  | Native _   -> Io.println "<builtin-function>"
;;

let _type_of (v : value) : string = (* TODO better type representation *)
  match v with
  | Int _     -> _int_type
  | Bool _    -> _bool_type
  | Closure _ -> _function_type
  | Native _  -> _function_type
;;

let _context_lookup (ctx : context) (name : string) : value option =
  Map.get name ctx
;;

let _context_insert (ctx : context) (name : string) (v : value) : context =
  Map.add name v ctx
;;

let _context_insert_pairs (ctx : context) (pairs : (string * value) list)
  : context =
  List.fold_left (fun ctx (n, v) -> _context_insert ctx n v) ctx pairs
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

(* ASSUME [opstr] is a primitive operator *)
let _interp_prim_op (ctx : context) (opstr : string) (expr_span : Span.t) : value =
  let args = ["a"; "b"] in  (* could cache this geneartion but whatever *)
  let expr_typ = None in
  let ident_expr = { Ast.expr_desc = Exp_ident opstr; expr_span; expr_typ } in
  let arg_exprs = List.map
      (fun str -> { Ast.expr_desc = Exp_ident str; expr_span; expr_typ }) args
  in
  let body_expr = { Ast.expr_desc = Exp_apply (ident_expr, arg_exprs);
                    expr_span; expr_typ  } in
  Closure (["a"; "b"], body_expr, ref ctx)
;;

let _interp_name (ctx : context) (name : string) (where : Span.t) : value =
  match _context_lookup ctx name with
  | Some v -> v
  | None ->
    match Primops.get_kind name with
    | None -> _error_unbound_var name where
    | Some _ -> _interp_prim_op ctx name where
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

and _interp_apply (ctx : context)
    (func : Ast.expression) (args : Ast.expression list) (span : Span.t)
  : value =
    (* inline primop application, or get stuck *)
  match func with
  | { expr_desc = Exp_ident name; expr_span; expr_typ = None } ->
    _interp_potential_primop_apply ctx name expr_span args span
  | _ ->
    let func_value = _interp_expr ctx func in
    _interp_apply_func_value ctx func_value func.expr_span args span

and _interp_potential_primop_apply (ctx : context)
    (func_ident : string) (func_span : Span.t) (args : Ast.expression list)
    (apply_span : Span.t)
  : value =
  match _context_lookup ctx func_ident with (* allow shadowing of built-in primops *)
  | Some value -> _interp_apply_func_value ctx value func_span args apply_span
  | None ->
    match Primops.get_kind func_ident with
    | None -> _error_unbound_var func_ident func_span
    | Some primop -> _interp_primop ctx primop args apply_span

and _interp_primop (ctx : context)
    (primop : Primops.op_kind) (args : Ast.expression list) (apply_span : Span.t)
  : value =
  match args with
  | lhs::rhs::[] -> _interp_binary_primop ctx primop lhs rhs
  | _ -> _error_arity_mismatch 2 (List.length args) apply_span

and _interp_binary_primop (ctx : context)
    (primop : Primops.op_kind) (lhs : Ast.expression) (rhs : Ast.expression)
  : value =
  match primop with
  | LogicAnd -> _interp_boolean_binop ctx lhs rhs SC_on_false
  | LogicOr  -> _interp_boolean_binop ctx lhs rhs SC_on_true
  | Equal  ->
    let lhs_v = _interp_expr ctx lhs in
    let rhs_v = _interp_expr ctx rhs in
    Bool (lhs_v = rhs_v)
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
    (func : value) (func_span : Span.t) (args : Ast.expression list)
    (apply_span : Span.t) : value =
  match func with
  | Closure (names, body, func_ctx) ->
    let argvs = List.map (_interp_expr ctx) args in
    let len_expect = List.length names in
    let len_actual = List.length argvs in
    if len_expect <> len_actual
    then _error_arity_mismatch len_expect len_actual apply_span
    else
      let name_arg_pairs = List.combine names argvs in
      let ctx = _context_insert_pairs !func_ctx name_arg_pairs in
      _interp_expr ctx body
  | Native func -> _interp_native_apply ctx func args apply_span
  | _ as v -> _error_type_mismatch _function_type (_type_of v) func_span

and _interp_native_apply (ctx : context)
    (func : builtin_func) (args : Ast.expression list) (span : Span.t) : value =
  match func with
  | Builtin_println ->
    match args with
    | [arg_e] ->
      let arg_v = _interp_expr ctx arg_e in
      _println_val arg_v;
      arg_v
    | _ -> _error_arity_mismatch 1 (List.length args) span
;;

let _interp_struct_item (ctx : context) (item : Ast.struct_item) : context =
  match item.struct_item_desc with
  | Struct_eval expr -> let _ = _interp_expr ctx expr in ctx
  | Struct_bind (rec_flag, bds) -> _interp_let_bindings ctx rec_flag bds
;;

let _interp_struct_impl items =
  let init_ctx = Map.empty String.compare in
  let init_ctx = Map.add "println" (Native Builtin_println) init_ctx in
  let _ = List.fold_left _interp_struct_item init_ctx items in
  ()
;;

let interp_struct items =
  try Ok (_interp_struct_impl items)
  with Ast_interp_error err -> Error err
;;
