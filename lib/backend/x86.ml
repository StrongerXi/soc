open Pervasives

type physical_reg =
  | Rax
  | Rbx
  | Rsi
  | Rdi
  | Rdx
  | Rcx
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15

let ordered_argument_physical_regs =
  [Rdi; Rsi; Rdx; Rcx; R8; R9]
;;

type 'a reg =
  | Rsp        
  | Rbp        
  | Greg of 'a 

type 'a arg =
  | Lbl_arg of Label.t
  | Imm_arg of int
  | Reg_arg of 'a reg
  | Mem_arg of 'a reg * int

type cond =
  | Eq
  | Lt

type binop =
  | Add
  | Sub
  | Mul

type 'a instr = 
  | Label of Label.t
  | Load of 'a arg * 'a reg
  | Store of 'a arg * 'a reg * int
  | Push of 'a
  | Pop of 'a
  | Binop of binop * 'a * 'a arg
  | Cmp of 'a arg * 'a
  | Jmp of Label.t
  | JmpC of cond * Label.t
  | Call_reg of 'a
  | Call_lbl of Label.t
  | SetC of cond * 'a
  | Ret

type func =
  { entry  : Label.t
  ; instrs : physical_reg instr list
  }

type prog = 
  { funcs : func list
  ; main : func
  }

type temp_func = 
  { entry  : Label.t
  ; instrs : Temp.t instr list
  ; args   : Temp.t list
  ; rax    : Temp.t
  }

type temp_prog = 
  { temp_funcs : temp_func list
  ; temp_main  : temp_func
  }


(* context for translation from [Lir.func] to [temp_func] *)
type context =
  { func_label        : Label.t

  (* following are used to encode calling convention *)
  ; rax_temp          : Temp.t
  ; ordered_arg_temps : Temp.t list 
    (* _All_ args that can fit in regs; NOTE Original func might not have this
     * many args, but calls to other func might need more args. It can also
     * have more, but we ignore the rest. *)

  (* following are "mutable" *)
  ; temp_manager      : Temp.manager
  ; rev_instrs        : Temp.t instr list
  }

let rec _generate_n_temps (manager : Temp.manager) (n : int)
  : (Temp.manager * Temp.t list) =
  if n <= 0 then (manager, [])
  else 
    let manager, n_sub_1_temps = _generate_n_temps manager (n - 1) in
    let manager, temp = Temp.gen manager in
    (manager, temp::n_sub_1_temps)
;;

(* expand or shrink [temps] so that output is a list of size [max(0, n)] *)
let rec _ensure_n_temps
    (manager : Temp.manager) (temps : Temp.t list) (n : int)
  : (Temp.manager * Temp.t list) =
  if n <= 0 then (manager, [])
  else
    match temps with
    | [] -> _generate_n_temps manager n
    | temp::temps ->
      let manager, n_sub_1_temps = _ensure_n_temps manager temps (n - 1) in
      (manager, temp::n_sub_1_temps)
;;

let _init_ctx
    (func_label : Label.t) (ordered_arg_temps : Temp.t list)
    (temp_manager : Temp.manager)
  : context =
  let temp_manager, rax_temp = Temp.gen temp_manager in
  let x86_arg_reg_num = List.length ordered_argument_physical_regs in
  let temp_manager, ordered_arg_temps =
    _ensure_n_temps temp_manager ordered_arg_temps x86_arg_reg_num in
  { func_label; rax_temp; ordered_arg_temps;
    temp_manager; rev_instrs = [] }
;;

let _ctx_add_instr (ctx : context) (instr : Temp.t instr) : context =
  let rev_instrs = instr::ctx.rev_instrs in
  { ctx with rev_instrs }
;;

let _ctx_get_instrs (ctx : context) : Temp.t instr list =
  List.rev ctx.rev_instrs
;;

let _ctx_gen_temp (ctx : context) : (context * Temp.t)  =
  let temp_manager, temp = Temp.gen ctx.temp_manager in
  ({ ctx with temp_manager }, temp)
;;

let _ctx_get_epilogue_label (ctx : context) : Label.t =
  Label.to_epilogue ctx.func_label
;;

(* only need to load extra args from stack.
 * Must synch with [_emit_prepare_x86_call_args] *)
let _emit_load_args_into_temps (ctx : context) (ordered_arg_temps : Temp.t list)
  : context =
  let load_extra_args ctx extra_arg_temps : context =
    List.fold_left
      (fun (ctx, offset_bytes) arg_temp ->
         let mem_arg = Mem_arg (Rbp, offset_bytes) in
         let instr = Load (mem_arg, Greg arg_temp) in
         let ctx = _ctx_add_instr ctx instr in
         (ctx, offset_bytes + Constants.word_size))
      (ctx, 2 * Constants.word_size) extra_arg_temps
    |> (fun (ctx, _) -> ctx)
  in
  let rec go ctx arg_temps (arg_prs : physical_reg list) : context =
    match arg_temps, arg_prs with
    | [], _ -> ctx
    | _, [] -> load_extra_args ctx arg_temps
    | _::rest_arg_temps, _::rest_arg_prs ->
      go ctx rest_arg_temps rest_arg_prs
  in
  go ctx ordered_arg_temps ordered_argument_physical_regs
;;

let rec _emit_lir_expr (ctx : context) (e : Lir.expr) (dst_temp : Temp.t)
  : context =
  match e with
  | Imm n -> 
    let instr = Load (Imm_arg n, Greg dst_temp) in
    _ctx_add_instr ctx instr

  | Tmp temp -> (* XXX generate to [arg] would save 1 instr *)
    let instr = Load (Reg_arg (Greg temp), Greg dst_temp) in
    _ctx_add_instr ctx instr

  | Op (op, lhs_e, rhs_e) ->
    _emit_lir_op ctx op lhs_e rhs_e dst_temp

  | Call (label_temp, es) ->
    let ctx = _emit_prepare_x86_call_args ctx es in
    let instr = Call_reg label_temp in
    let ctx = _ctx_add_instr ctx instr in
    _ctx_add_instr ctx (Load (Reg_arg (Greg ctx.rax_temp), Greg dst_temp))

  | NativeCall (func_label, es) ->
    let ctx = _emit_prepare_x86_call_args ctx es in
    let instr = Call_lbl func_label in
    let ctx = _ctx_add_instr ctx instr in
    _ctx_add_instr ctx (Load (Reg_arg (Greg ctx.rax_temp), Greg dst_temp))

  | Mem_alloc nbytes ->
    let ctx = _emit_prepare_x86_call_args ctx [Imm nbytes] in
    let instr = Call_lbl (Label.get_native Constants.mem_alloc_name) in
    let ctx = _ctx_add_instr ctx instr in
    _ctx_add_instr ctx (Load (Reg_arg (Greg ctx.rax_temp), Greg dst_temp))

(*       ......
 * caller stack frame
 * ------------------
 * | 1st extra arg  |
 * ------------------ <- RBP + 2 * word_size
 * | return address |
 * ------------------ <- RBP + 1 * word_size
 * |    saved rbp   | 
 * ------------------ <- RBP + 0 * word_size 
 * callee stack frame
 *      ......        *)
and _emit_prepare_x86_call_args (init_ctx : context)
    (init_arg_es : Lir.expr list)
  : context =
  (* last argument is pushed first, so 1st arg is closest to frame base *)
  let _emit_push_onto_stack ctx arg_es =
    List.fold_right
      (fun arg_e ctx ->
         let ctx, arg_v_temp = _ctx_gen_temp ctx in
         let ctx = _emit_lir_expr ctx arg_e arg_v_temp in
         _ctx_add_instr ctx (Push arg_v_temp))
      arg_es ctx
  in
  let rec go ctx arg_es (ordered_arg_temps : Temp.t list)  =
    match arg_es, ordered_arg_temps  with
    | [], _ -> ctx
    | _, [] -> _emit_push_onto_stack ctx arg_es
    | arg_e::rest_arg_es, arg_temp::rest_arg_temps ->
      let ctx = _emit_lir_expr ctx arg_e arg_temp in
      go ctx rest_arg_es rest_arg_temps
  in
  let x86_arg_temps =
    List.combine init_ctx.ordered_arg_temps ordered_argument_physical_regs
    |> List.map (fun (temp, _) -> temp)
  in
  go init_ctx init_arg_es x86_arg_temps

and _emit_lir_op (ctx : context)
    (op : Lir.op) (lhs_e : Lir.expr) (rhs_e : Lir.expr) (dst_temp : Temp.t)
  : context =
  let ctx, rhs_temp = _ctx_gen_temp ctx in
  let ctx = _emit_lir_expr ctx lhs_e dst_temp in
  let ctx = _emit_lir_expr ctx rhs_e rhs_temp in
  let binop =
    match op with
    | Add -> Add
    | Sub -> Sub
    | Mul -> Mul
  in
  let instr = Binop (binop, dst_temp, Reg_arg (Greg rhs_temp)) in
  _ctx_add_instr ctx instr
;;

let _emit_comparison
    (ctx : context) (lhs_e : Lir.expr) (rhs_e : Lir.expr)
    : context =
  let ctx, lhs_temp = _ctx_gen_temp ctx in
  let ctx, rhs_temp = _ctx_gen_temp ctx in
  let ctx = _emit_lir_expr ctx lhs_e lhs_temp in
  let ctx = _emit_lir_expr ctx rhs_e rhs_temp in
  let instr = Cmp (Reg_arg (Greg lhs_temp), rhs_temp) in
  _ctx_add_instr ctx instr
;;

let _emit_lir_jump
    (ctx : context) (lir_cond : Lir.cond) (target_label : Label.t)
  : context =
  match lir_cond with
  | True -> 
    _ctx_add_instr ctx (Jmp target_label)

  | Less (lhs_e, rhs_e) ->
    let ctx = _emit_comparison ctx lhs_e rhs_e in
    let instr = JmpC (Lt, target_label) in
    _ctx_add_instr ctx instr

  | Equal (lhs_e, rhs_e) ->
    let ctx = _emit_comparison ctx lhs_e rhs_e in
    let instr = JmpC (Eq, target_label) in
    _ctx_add_instr ctx instr
;;

let _emit_lir_set
    (ctx : context) (lir_cond : Lir.cond) (target_temp : Temp.t)
  : context =
  match lir_cond with
  | True -> 
    let instr = Load (Imm_arg 1, Greg target_temp) in
    _ctx_add_instr ctx instr

  | Less (lhs_e, rhs_e) ->
    let ctx = _emit_comparison ctx lhs_e rhs_e in
    let instr = SetC (Lt, target_temp) in
    _ctx_add_instr ctx instr

  | Equal (lhs_e, rhs_e) ->
    let ctx = _emit_comparison ctx lhs_e rhs_e in
    let instr = SetC (Eq, target_temp) in
    _ctx_add_instr ctx instr
;;


let _emit_lir_instr (ctx : context) (lir_instr : Lir.instr) : context =
  (* TODO this could generate seriously inefficient code.
   * To optimize, e.g., consider emit certain expr into arg instead of reg *)
  match lir_instr with
  | Label label ->
    _ctx_add_instr ctx (Label label)

  | Load (e, dst_temp) ->
    _emit_lir_expr ctx e dst_temp

  | LoadMem (src_addr_e, dst_temp) ->
    let ctx = _emit_lir_expr ctx src_addr_e dst_temp in
    let instr = Load (Mem_arg(Greg dst_temp, 0), Greg dst_temp) in
    _ctx_add_instr ctx instr

  | Store (e, dst_addr_e) ->
    let ctx, e_temp = _ctx_gen_temp ctx in
    let ctx, dst_addr_temp = _ctx_gen_temp ctx in
    let ctx = _emit_lir_expr ctx e e_temp in
    let ctx = _emit_lir_expr ctx dst_addr_e dst_addr_temp in
    let instr = Store (Reg_arg(Greg e_temp), Greg dst_addr_temp, 0) in
    _ctx_add_instr ctx instr

  | Store_label (label, dst_addr_e) ->
    let ctx, dst_addr_temp = _ctx_gen_temp ctx in
    let ctx = _emit_lir_expr ctx dst_addr_e dst_addr_temp in
    let instr = Store (Lbl_arg(label), Greg dst_addr_temp, 0) in
    _ctx_add_instr ctx instr

  | Jump (lir_cond, target_label) ->
    _emit_lir_jump ctx lir_cond target_label

  | Set (lir_cond, target_temp) ->
    _emit_lir_set ctx lir_cond target_temp

  | Ret e ->
    let ctx = _emit_lir_expr ctx e ctx.rax_temp in
    let epilogue_label = _ctx_get_epilogue_label ctx in
    _ctx_add_instr ctx (Jmp epilogue_label)
;;

let _emit_lir_instrs (ctx : context) (lir_instrs : Lir.instr list) : context =
  List.fold_left _emit_lir_instr ctx lir_instrs
;;

let _from_lir_func (lir_func : Lir.func) : temp_func =
  let args = lir_func.ordered_args in
  let ctx = _init_ctx lir_func.name args lir_func.temp_manager in
  let ctx = _emit_load_args_into_temps ctx args in
  let ctx = _emit_lir_instrs ctx lir_func.body in
  let instrs = _ctx_get_instrs ctx in
  let func_label = lir_func.name in
  let func = { entry = func_label; instrs; args; rax = ctx.rax_temp; } in
  func
;;

let _from_lir_main_func
    (temp_manager : Temp.manager) (lir_instrs : Lir.instr list)
  : temp_func =
  (* TODO call "soml_init" here, or make this a function called by C runtime?
   * TBD when implementing runtime. *)
  let entry_label = Label.get_native Constants.entry_name in
  let ctx = _init_ctx entry_label [] temp_manager in
  let ctx = _emit_lir_instrs ctx lir_instrs in
  let instrs = _ctx_get_instrs ctx in
  { entry = entry_label; instrs; args = []; rax = ctx.rax_temp; } 
;;

let from_lir_prog (lir_prog : Lir.prog) : temp_prog =
  let funcs = List.map _from_lir_func lir_prog.funcs in
  let temp_main = _from_lir_main_func lir_prog.temp_manager lir_prog.entry in
  { temp_funcs = funcs; temp_main }
;;
