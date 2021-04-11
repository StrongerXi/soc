open Pervasives

type op =
  | Add
  | Sub
  | Mul

type expr =
  | Imm of int                           
  | Tmp of Temp.t                        
  | Op of op * expr * expr 
  | Call of Temp.t * expr list
  | NativeCall of Label.t * expr list
  | Mem_alloc of int

type cond =
  | True
  | Less of expr * expr
  | Equal of expr * expr

type instr =
  | Label of Label.t
  | Load of expr * Temp.t
  | LoadMem of expr * Temp.t
  | Store of expr * expr
  | Store_label of Label.t * expr
  | Jump of cond * Label.t
  | Set of cond * Temp.t
  | Ret of expr

type func =
  { name     : Label.t
  ; args     : Temp.t list
  ; body     : instr list 
  }

type prog =
  { funcs : func list
  ; entry : instr list
  }


(* Context for translating a Cir funciton, hiding boiler-plate details. *)
type context =
  { temp_manager  : Temp.manager
  ; label_manager : Label.manager
  ; rev_instrs    : instr list
  }

let _ctx_init (label_manager : Label.manager) : context = 
  { temp_manager = Temp.init_manager
  ; label_manager
  ; rev_instrs = []
  }
;;

(* Some wrappers for convenience *)
let _ctx_add_instr (ctx : context) (instr : instr) : context =
  let rev_instrs = instr::ctx.rev_instrs in
  { ctx with rev_instrs }
;;

(* Add in order *)
let _ctx_add_instrs (ctx : context) (instrs : instr list) : context =
  let rec go rev_instrs instrs_to_add =
    match instrs_to_add with
    | [] -> rev_instrs
    | next_to_add::rest_to_add -> go (next_to_add::rev_instrs) rest_to_add
  in
  let rev_instrs = go ctx.rev_instrs instrs in
  { ctx with rev_instrs }
;;

let _ctx_get_instrs (ctx : context) : instr list =
  List.rev ctx.rev_instrs
;;

let _ctx_get_label (ctx : context) (s : string) : Label.t =
  match Label.get ctx.label_manager s with
  | None -> failwith "[Lir._ctx_get_label] Unbound name for label"
  | Some label -> label
;;

let _ctx_gen_label (ctx : context) (prefix : string) : (context * Label.t) =
  let label_manager, label = Label.gen ctx.label_manager prefix in
  let ctx = { ctx with label_manager } in
  (ctx, label)
;;

let _ctx_gen_and_bind_label (ctx : context) (name : string)
  : (context * Label.t) =
  let label_manager, label = Label.gen_and_bind ctx.label_manager name in
  let ctx = { ctx with label_manager } in
  (ctx, label)
;;

let _ctx_get_temp (ctx : context) (ident : string) : Temp.t =
  match Temp.get ctx.temp_manager ident with
  | None -> failwith "[Lir._ctx_get_label] Unbound identifier"
  | Some temp -> temp
;;

let _ctx_gen_temp (ctx : context) : (context * Temp.t) =
  let temp_manager, temp = Temp.gen ctx.temp_manager in
  let ctx = { ctx with temp_manager } in
  (ctx, temp)
;;

let _ctx_gen_and_bind_temp (ctx : context) (ident : string)
  : (context * Temp.t) =
  let temp_manager, temp = Temp.gen_and_bind ctx.temp_manager ident in
  let ctx = { ctx with temp_manager } in
  (ctx, temp)
;;



(* Some constants *)
let _word_size = 8 (* A bit brittle, good for now I guess, I hope *)
and _true_e    = Imm 1
and _false_e   = Imm 0
;;


(* Translate [ce] and emit instructions into [ctx].
 * ASSUME [ce] is at tail position. *)
let rec _transl_cir_expr_tailpos (ctx : context) (ce : Cir.expr) : context =
  let ctx, e = _transl_cir_expr ctx ce in
  _ctx_add_instr ctx (Ret e)

and _transl_cir_expr (ctx : context) (ce : Cir.expr) : (context * expr) =
  match ce with
  | Cconst const               -> (ctx, _transl_cir_const const)
  | Cident name                -> (ctx, Tmp (_ctx_get_temp ctx name))
  | Cmk_closure mkcls          -> _transl_cir_mk_closure ctx mkcls
  | Cprimop (op_kind, args)    -> _transl_cir_primop ctx op_kind args
  | Cif (cnd, thn, els)        -> _transl_cir_if ctx cnd thn els
  | Capply (func, args)        -> _transl_cir_apply ctx func args
  | Cnative_apply (func, args) -> _transl_cir_native_apply ctx func args

  | Clet (bds, body) ->
    let ctx = _transl_cir_let_bindings ctx bds in
    _transl_cir_expr ctx body

  | Cletrec (bds, body) ->
    let ctx = _transl_cir_letrec_bindings ctx bds in
    _transl_cir_expr ctx body

and _transl_cir_const (const : Cir.constant) : expr =
  match const with
  | CInt n      -> Imm (n * 2 + 1) (* NOTE synch with C runtime *)
  | CBool true  -> _true_e
  | CBool false -> _false_e

and _transl_cir_primop (ctx : context)
    (op_kind : Primops.op_kind) (args : Cir.expr list)
  : (context * expr) =
  let lhs_ce, rhs_ce = match args with
    | lhs_ce::[rhs_ce] -> lhs_ce, rhs_ce
    | _ -> failwith "[Lir._transl_cir_primop] invalid # of args for primop"
  in

  (* can't evaluate both due to potential short-circuiting *)
  let ctx, lhs_e = _transl_cir_expr ctx lhs_ce in 
  match op_kind with
  (* NOTE there are small optimizations
   * synch with representations defined in [_transl_cir_const] *)
  | AddInt ->
    let ctx, rhs_e = _transl_cir_expr ctx rhs_ce in
    (* (2a + 1) + (2b + 1) = 2(a+b) + 2 = (2(a+b) + 1) + 1 *)
    (ctx, Op (Sub, (Op (Add, lhs_e, rhs_e)), Imm 1))

  | SubInt ->
    let ctx, rhs_e = _transl_cir_expr ctx rhs_ce in
    (* (2a + 1) - (2b + 1) = 2(a-b) = (2(a+b) - 1) + 1 *)
    (ctx, Op (Add, (Op (Add, lhs_e, rhs_e)), Imm 1))

  | MulInt ->
    let ctx, rhs_e = _transl_cir_expr ctx rhs_ce in
    (* (2a + 1) * (2b + 1) = 4(a*b) + 2(a+b) + 2 = 4(a*b) + 1 + (2(a+b) + 1) *)
    let lhs_add_rhs = Op (Add, lhs_e, rhs_e) in
    let extra = Op (Add, Imm 1, (Op (Mul, Imm 2, lhs_add_rhs))) in
    let lhs_mul_rhs = Op (Mul, lhs_e, rhs_e) in
    (ctx, Op (Sub, lhs_mul_rhs, extra))

  (* TODO could optimize And/Or if it's in an if condition -- jump straight
   * to corresponding branch when short-circuiting *)
  | LogicAnd ->
    _transl_binop_with_short_circuit ctx lhs_e rhs_ce _false_e "and"

  | LogicOr ->
    _transl_binop_with_short_circuit ctx lhs_e rhs_ce _true_e "or"

  | LtInt ->
    let ctx, rhs_e = _transl_cir_expr ctx rhs_ce in
    let ctx, result_temp = _ctx_gen_temp ctx in
    let set_instr = Set (Less (lhs_e, rhs_e), result_temp) in
    let ctx = _ctx_add_instr ctx set_instr in
    (ctx, Tmp result_temp)
    
(* Macro to help codegen:
 *
 * if lhs = {value} jump to {prefix}-short-circuit
 * [...translate rhs...]
 * result_temp := rhs_expr
 * jump to {prefix}-end
 *
 * {prefix}-short-circuit:
 * result_temp := {value}
 *
 * {prefix}-end: ... *)
and _transl_binop_with_short_circuit
    (ctx : context) (lhs_e : expr) (rhs_ce : Cir.expr)
    (value : expr) (prefix : string)
  : (context * expr) =
  let ctx, short_circuit_label =
    _ctx_gen_label ctx (String.append prefix "_short_circuit") in
  let ctx, end_label = _ctx_gen_label ctx (String.append prefix "_end") in
  let ctx, result_temp = _ctx_gen_temp ctx in
  let lhs_eq_value_e = Equal (lhs_e, value) in
  let ctx = _ctx_add_instr ctx (Jump (lhs_eq_value_e, short_circuit_label)) in
  let ctx, rhs_e = _transl_cir_expr ctx rhs_ce in
  let ctx = _ctx_add_instrs ctx [ Load (rhs_e, result_temp);
                                  Jump (True, end_label);
                                  Label short_circuit_label;
                                  Load (value, result_temp);
                                  Label end_label; ]
  in
  (ctx, Tmp result_temp)

(* [...translate cnd...]
 * If cond_expr = 0 jump to false
 * [...translate thn...]
 * result_temp := thn_expr
 * jump to end
 *
 * false:
 * [...translate els...]
 * result_temp := els_expr
 *
 * end: ...
 *)
and _transl_cir_if
    (ctx : context) (cnd_ce : Cir.expr) (thn_ce : Cir.expr) (els_ce : Cir.expr)
  : (context * expr) =
  let ctx, result_temp = _ctx_gen_temp ctx in
  let ctx, false_label = _ctx_gen_label ctx "if_false" in
  let ctx, end_label = _ctx_gen_label ctx "if_end" in
  let ctx, cnd_e = _transl_cir_expr ctx cnd_ce in
  let cond_eq_false_e = Equal (cnd_e, _false_e) in
  let ctx = _ctx_add_instr ctx (Jump (cond_eq_false_e, false_label)) in
  let ctx, thn_e = _transl_cir_expr ctx thn_ce in
  let ctx = _ctx_add_instr ctx (Load (thn_e, result_temp)) in
  let ctx = _ctx_add_instr ctx (Jump (cond_eq_false_e, end_label)) in
  let ctx = _ctx_add_instr ctx (Label false_label) in
  let ctx, els_e = _transl_cir_expr ctx els_ce in
  let ctx = _ctx_add_instr ctx (Load (els_e, result_temp)) in
  let ctx = _ctx_add_instr ctx (Label end_label) in
  (ctx, Tmp result_temp)

(* NOTE must synch up with [_emit_cls_prelude]
 *
 * [...translate func_ce...]
 * [...translate arg_cen...]
 * label_temp := *[func_e]
 * Call(label_temp, [func_e; arg_e0; ... arg_en])
 *)
and _transl_cir_apply
    (ctx : context) (func_ce : Cir.expr) (arg_ces : Cir.expr list)
  : (context * expr) =
  let ctx, func_e = _transl_cir_expr ctx func_ce in
  let ctx, arg_es = _transl_cir_apply_args ctx arg_ces in
  let ctx, label_temp = _ctx_gen_temp ctx in
  let ctx = _ctx_add_instr ctx (LoadMem (func_e, label_temp)) in
  let call_e = Call (label_temp, func_e::arg_es) in
  (ctx, call_e)

and _transl_cir_native_apply
    (ctx : context) (name : string) (arg_ces : Cir.expr list)
  : (context * expr) =
  let native_label = Label.create_native name in
  let ctx, arg_es = _transl_cir_apply_args ctx arg_ces in
  let call_e = NativeCall (native_label, arg_es) in
  (ctx, call_e)

and _transl_cir_apply_args (ctx : context) (arg_ces : Cir.expr list)
  : (context * expr list) =
  (* NOTE technically the order can be arbitrary :) *)
  List.fold_left 
    (fun (ctx, rev_arg_es) arg_ce ->
       let ctx, arg_e = _transl_cir_expr ctx arg_ce in
       (ctx, arg_e::rev_arg_es))
    (ctx, []) arg_ces
  |> (fun (ctx, rev_arg_es) -> (ctx, List.rev rev_arg_es))

(* [...translate bd_rhs_0...]
 * result_temp_0 := bd_rhs_0
 * ...
 * [...translate bd_rhs_n...]
 * result_temp_n := bd_rhs_n
 *)
and _transl_cir_let_bindings
    (ctx : context) (bds : (string * Cir.expr) list)
  : context =
  List.fold_left 
    (fun ctx (lhs_name, rhs_ce) ->
       let ctx, result_temp = _ctx_gen_and_bind_temp ctx lhs_name in
       let ctx, rhs_e = _transl_cir_expr ctx rhs_ce in
       _ctx_add_instr ctx (Load (rhs_e, result_temp)))
    ctx bds

(* [...translate bd_rhs_0 by allocating space only...]
 * result_temp_0 := bd_rhs_0
 * ...
 * [...translate bd_rhs_n by allocating space only...]
 * result_temp_n := bd_rhs_n
 * [...finish translating bd_rhs_0...]
 * ...
 * [...finish translating bd_rhs_n...]
 *)
and _transl_cir_letrec_bindings
    (ctx : context) (bds : (string * Cir.letrec_rhs) list)
  : context =
  let ctx, (addr_rhs_pairs : (expr * Cir.letrec_rhs) list)  =
    List.fold_left 
      (fun (ctx, rev_addr_rhs_pairs) (lhs_name, rhs_ce) ->
         let ctx, result_temp = _ctx_gen_and_bind_temp ctx lhs_name in
         let addr_e = _transl_cir_letrec_rhs_alloc_space rhs_ce in
         let ctx = _ctx_add_instr ctx (Load (addr_e, result_temp)) in
         let pair = (Tmp result_temp, rhs_ce) in
         (ctx, pair::rev_addr_rhs_pairs))
      (ctx, []) bds
  |> (fun (ctx, rev_addr_rhs_pairs) -> (ctx, List.rev rev_addr_rhs_pairs))
  in 
  List.fold_left
    (fun ctx (addr_e, rhs_ce) ->
       let ctx, _ = _transl_cir_letrec_rhs_skip_alloc ctx addr_e rhs_ce in
       ctx)
    ctx addr_rhs_pairs

and _transl_cir_letrec_rhs_alloc_space (rhs_ce : Cir.letrec_rhs) : expr =
  match rhs_ce with
  | Rhs_const const -> _transl_cir_const const
  | Rhs_mkcls mkcls -> _transl_cir_mkcls_alloc_space mkcls.free_vars

and _transl_cir_letrec_rhs_skip_alloc
    (ctx : context) (addr_e : expr) (rhs_ce : Cir.letrec_rhs)
  : (context * expr) =
  match rhs_ce with
  | Rhs_const _ -> (ctx, addr_e) (* unboxed value needs no extra space *)
  | Rhs_mkcls mkcls -> _transl_cir_mkcls_skip_alloc ctx addr_e mkcls

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++
 * | entry label | first freevar | ... | last freevar |
 * ++++++++++++++++++++++++++++++++++++++++++++++++++++
 * Each slot takes up a word size
 * NOTE must synch up with [_emit_cls_prelude] *)
and _transl_cir_mkcls_alloc_space (free_vars : string list) : expr =
  let slots = 1 + (List.length free_vars) in
  Mem_alloc slots

and _transl_cir_mkcls_skip_alloc (ctx : context)
    (cls_addr_e : expr) (mkcls : Cir.mk_closure)
  : (context * expr) =
  let entry_label = _ctx_get_label ctx mkcls.func_name in
  let ctx = _ctx_add_instr ctx (Store_label (entry_label, cls_addr_e)) in
  let instrs =
    List.mapi 
      (fun n fv_name -> (* store each free variable to their slot *)
         let word_offset = n + 1 in (* start at 1 *)
         let fv_e = Tmp (_ctx_get_temp ctx fv_name) in
         let slot_addr_e =
           Op (Add, cls_addr_e, Imm (word_offset * _word_size)) in
         Store (fv_e, slot_addr_e))
      mkcls.free_vars
  in
  let ctx = _ctx_add_instrs ctx instrs in
  (ctx, cls_addr_e)

and _transl_cir_mk_closure (ctx : context) (mkcls : Cir.mk_closure)
  : (context * expr) =
  let space_alloc_e = _transl_cir_mkcls_alloc_space mkcls.free_vars in
  let ctx, cls_addr_temp = _ctx_gen_temp ctx in
  let ctx = _ctx_add_instr ctx (Load (space_alloc_e, cls_addr_temp)) in
  let cls_addr_e = Tmp cls_addr_temp in
  _transl_cir_mkcls_skip_alloc ctx cls_addr_e mkcls
;;


(* Generate temps for args; load closure free variables into temps.
 * Return an list of temps for arguments __in order__.
 * NOTE
 * - Must be used at beginning of a closure translation. 
 * - Must synch up [_transl_cir_apply] and [_transl_cir_mk_closure]. *)
let _emit_cls_prelude (ctx : context) (cls : Cir.closure)
  : (context * Temp.t list) =
  let ctx, cls_temp = _ctx_gen_temp ctx in
  let ctx, arg_temps =
    List.fold_right
      (fun arg_name (ctx, arg_temps) ->
         let ctx, arg_temp = _ctx_gen_and_bind_temp ctx arg_name in
         (ctx, arg_temp::arg_temps))
      cls.args
      (ctx, [])
  in
  let ctx, _ = (* emit code for loading freevars *)
    List.fold_left (* start with first freevar *)
      (fun (ctx, word_offset) fv_name ->
         (* fv_temp = *[cls_temp + word_offset] *)
         let ctx, fv_temp = _ctx_gen_and_bind_temp ctx fv_name in
         let slot_addr_e = Op (Add, Tmp cls_temp, Imm word_offset) in
         let load_instr = LoadMem (slot_addr_e, fv_temp) in
         let ctx = _ctx_add_instr ctx load_instr in
         (ctx ,word_offset + 1))
      (ctx, 1) (* offset 0 is entry label *)
      cls.free_vars
  in
  (ctx, cls_temp::arg_temps)
;;
  

(* ASSUME each func name from the Cir program is bound in [label_manager] *)
let _from_closure
    (label_manager : Label.manager) (name : string) (cls : Cir.closure)
  : (Label.manager * func) =
  let ctx = _ctx_init label_manager in
  let entry_label = _ctx_get_label ctx name in
  let ctx, arg_temps = _emit_cls_prelude ctx cls in
  let ctx = _transl_cir_expr_tailpos ctx cls.body in
  let body = _ctx_get_instrs ctx in
  let func = { name = entry_label; args = arg_temps; body } in
  (ctx.label_manager, func)
;;

let from_cir_prog (cir_prog : Cir.prog) =
  let label_manager = Label.init_manager in
  (* Scope of each function name is the entire program *)
  let label_manager =
    Map.foldi
      (fun name _ label_manager ->
         let label_manager, _ = Label.gen_and_bind label_manager name in
         label_manager)
      cir_prog.funcs label_manager
  in
  let label_manager, funcs =
    Map.foldi
      (fun name cls (label_manager, funcs) ->
         let label_manager, func = _from_closure label_manager name cls in
         (label_manager, func::funcs))
      cir_prog.funcs (label_manager, [])
  in
  let ctx = _ctx_init label_manager in
  let ctx = _transl_cir_expr_tailpos ctx cir_prog.expr in
  let entry = _ctx_get_instrs ctx in
  { funcs; entry }
;;
