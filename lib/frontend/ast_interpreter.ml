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


let _println_val (v : value) : unit =
  match v with
  | Int n      -> Io.println (Int.to_string n)
  | Bool true  -> Io.println "true"
  | Bool false -> Io.println "false"
  | Closure _  -> Io.println "<function>"
  | Native _   -> Io.println "<builtin-function>"
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


type _short_circuit = (* for evaluating boolean ops *)
  | SC_on_true
  | SC_on_false

let _interp_const (const : Ast.constant) : value =
  match const with
  | Const_Int n -> Int n
  | Const_Bool b -> Bool b
;;

let _interp_name (ctx : context) (name : string) (where : Span.t) : value =
  match _context_lookup ctx name with
  | None -> failwith "TODO unbound name"
  | Some v -> v
;;

let rec _interp_expr (ctx : context) (expr : Ast.expression) : value =
  match expr.expr_desc with
  | Exp_const const -> _interp_const const
  | Exp_ident name  -> _interp_name ctx name expr.expr_span
  | Exp_fun (arg_names, body) ->
    let names =
      List.map (fun (x : Ast.opt_typed_var) -> x.var.stuff) arg_names in
    Closure (names, body, ref ctx)
  | Exp_binop (binop, lhs, rhs)   -> _interp_binop_expr ctx binop lhs rhs
  | Exp_if (cnd, thn, els)        -> _interp_if_expr    ctx cnd thn els
  | Exp_let (rec_flag, bds, body) -> _interp_let_expr   ctx rec_flag bds body
  | Exp_apply (func, args)        -> _interp_apply      ctx func args

and _interp_binop_expr (ctx : context)
    (op : Ast.binary_op) (lhs : Ast.expression) (rhs : Ast.expression) : value =
  match op with
  | Binop_and -> _interp_boolean_binop ctx lhs rhs SC_on_false
  | Binop_or -> _interp_boolean_binop ctx lhs rhs SC_on_true
  | _ ->
    let lhs_v = _interp_expr ctx lhs in
    let rhs_v = _interp_expr ctx rhs in
    match op, lhs_v, rhs_v with
    | Binop_add, Int n1, Int n2 -> Int (n1 + n2)
    | Binop_sub, Int n1, Int n2 -> Int (n1 - n2)
    | Binop_mul, Int n1, Int n2 -> Int (n1 * n2)
    | Binop_less, Int n1, Int n2 -> Bool (n1 < n2)
    | Binop_eq, _, _ -> Bool (lhs_v = rhs_v)
    | _ -> failwith "TODO Type mismatch"

and _interp_boolean_binop (ctx : context)
    (lhs : Ast.expression) (rhs : Ast.expression) (sc : _short_circuit)
  : value =
  let lhs_v = _interp_expr ctx lhs in
  match lhs_v, sc with
  | Bool true, SC_on_true -> lhs_v
  | Bool false, SC_on_false -> lhs_v
  | Bool b, _ ->
    begin
      match _interp_expr ctx rhs with
      | Bool b -> Bool b
      | _ -> failwith "expected bool TODO"
    end
  | _ -> failwith "expected bool TODO"

and _interp_if_expr (ctx : context)
    (cnd : Ast.expression) (thn : Ast.expression) (els : Ast.expression)
  : value =
  match _interp_expr ctx cnd with
  | Bool true -> _interp_expr ctx thn
  | Bool false -> _interp_expr ctx els
  | _ -> failwith "expected boolean TODO"

and _interp_let_expr (ctx : context)
    (rec_flag : Ast.rec_flag) (bds : Ast.binding list) (body : Ast.expression)
  : value =
  let ctx = _interp_let_bindings ctx rec_flag bds in
  _interp_expr ctx body

and _interp_let_bindings (ctx : context)
    (rec_flag : Ast.rec_flag) (bds : Ast.binding list) : context =
  let _interp_one_binding (b : Ast.binding) : (string * value) =
    let name = b.binding_lhs.var.stuff in
    match b.binding_rhs.expr_desc with
    | Exp_const _ | Exp_fun _ -> (* value restriction *)
      let rhs_v = _interp_expr ctx b.binding_rhs in (name, rhs_v)
    | _ -> failwith "TODO bad rhs for rec"
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
    (func : Ast.expression) (args : Ast.expression list) : value =
  match _interp_expr ctx func with
  | Closure (names, body, func_ctx) ->
    let argvs = List.map (_interp_expr ctx) args in
    let len_expect = List.length names in
    let len_actual = List.length argvs in
    if len_expect <> len_actual then failwith "TODO arity error"
    else
      let name_arg_pairs = List.combine names argvs in
      let ctx = _context_insert_pairs !func_ctx name_arg_pairs in
      _interp_expr ctx body
  | Native func ->
    _interp_native_apply ctx func args
  | _ -> failwith "TODO expected closure"

and _interp_native_apply (ctx : context)
    (func : builtin_func) (args : Ast.expression list) : value =
  let argvs = List.map (_interp_expr ctx) args in
  match func with
  | Builtin_println ->
    match argvs with
    | [v] -> _println_val v; v
    | _ -> failwith "TODO arity error"
;;

let _interp_struct_item (ctx : context) (item : Ast.struct_item) : context =
  match item.struct_item_desc with
  | Struct_eval expr -> let _ = _interp_expr ctx expr in ctx
  | Struct_bind (rec_flag, bds) -> _interp_let_bindings ctx rec_flag bds
;;

let interp_struct items =
  let init_ctx = Map.empty String.compare in
  let init_ctx = Map.add "println" (Native Builtin_println) init_ctx in
  let _ = List.fold_left _interp_struct_item init_ctx items in
  ()
;;
