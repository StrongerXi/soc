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
  | Store of 'a reg * 'a reg * int
  | Push of 'a reg
  | Pop of 'a reg
  | Binop of binop * 'a reg * 'a arg
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
         _ctx_add_instr ctx (Push (Greg arg_v_temp)))
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
  let instr = Binop (binop, Greg dst_temp, Reg_arg (Greg rhs_temp)) in
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
    let instr = Store (Greg e_temp, Greg dst_addr_temp, 0) in
    _ctx_add_instr ctx instr

  | Store_label (label, dst_addr_e) ->
    let ctx, dst_addr_temp = _ctx_gen_temp ctx in
    let ctx = _emit_lir_expr ctx dst_addr_e dst_addr_temp in
    let ctx, label_temp = _ctx_gen_temp ctx in
    let load_label_i = Load (Lbl_arg(label), Greg label_temp) in
    let store_label_i = Store (Greg label_temp, Greg dst_addr_temp, 0) in
    let ctx = _ctx_add_instr ctx load_label_i in
    _ctx_add_instr ctx store_label_i

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


let _add_temps_in_temp_reg (acc : Temp.t list) (reg : Temp.t reg)
  : Temp.t list =
  match reg with
  | Rsp | Rbp -> acc
  | Greg temp -> temp::acc
;;

let _add_temps_in_temp_arg  (acc : Temp.t list) (arg : Temp.t arg)
  : Temp.t list =
  match arg with
  | Lbl_arg _        -> acc
  | Imm_arg _        -> acc
  | Reg_arg reg      -> _add_temps_in_temp_reg acc reg
  | Mem_arg (reg, _) -> _add_temps_in_temp_reg acc reg
;;

let _get_reads_and_writes_temp_instr (rax : Temp.t) (instr : Temp.t instr)
  : (Temp.t list * Temp.t list) =
  match instr with
  | Label _          -> ([], [])
  | Jmp _            -> ([], [])
  | JmpC (_, _)      -> ([], [])
  | SetC (_, temp)   -> ([], [temp])

  | Push reg -> 
    let reads = _add_temps_in_temp_reg [] reg in
    (reads, [])

  | Pop reg  ->
    let writes = _add_temps_in_temp_reg [] reg in
    ([], writes)

  | Load (arg, dst_reg) ->
    let reads = _add_temps_in_temp_arg [] arg in
    let writes = _add_temps_in_temp_reg [] dst_reg in
    (reads, writes)

  | Store (src_reg, dst_addr_reg, _) ->
    let reads = _add_temps_in_temp_reg [] src_reg in
    let writes = _add_temps_in_temp_reg [] dst_addr_reg in
    (reads, writes)

  | Binop (_, reg, arg) ->
    let reg_temps = _add_temps_in_temp_reg [] reg in
    let reads = _add_temps_in_temp_arg reg_temps arg in
    (reads, reg_temps)

  | Cmp (arg, temp) ->
    let reads = _add_temps_in_temp_arg [temp] arg in
    let writes = [] in
    (reads, writes)

  (* Q: What about caller-saved registers?
   * A: We ignore all details about calling convention at this level (except
   *    for RAX, because our instr doesn't specify "dst_reg" of call).
   *    Basically we leave it to the register allocator to ensure that
   *    caller-saved registers are properly assigned or spilled *)
  | Call_reg temp -> ([temp], [rax])
  | Call_lbl _    -> ([],     [rax])
  | Ret           -> ([rax],  [])
;;

let _temp_instr_to_vasm (rax : Temp.t) (instr : Temp.t instr) : Vasm.t =
  let reads, writes = _get_reads_and_writes_temp_instr rax instr in
  match instr with
  | Label label -> Vasm.mk_label label

  | Load _ | Store _ | Push _ | Pop _ | Binop _ | Cmp _ | SetC _ ->
    Vasm.mk_instr reads writes

  | Call_reg _ | Call_lbl _ ->
    Vasm.mk_call reads writes

  | Jmp target -> Vasm.mk_dir_jump reads writes target
  | JmpC (_, target) -> Vasm.mk_cond_jump reads writes target

  | Ret -> Vasm.mk_ret reads writes
;;

let temp_func_to_vasms temp_func =
  let body_vasms =
    List.map (_temp_instr_to_vasm temp_func.rax) temp_func.instrs in
  let entry_label = Vasm.mk_label temp_func.entry in
  entry_label::body_vasms
;;


let _get_max_rbp_offset_arg (arg : 'a arg) : int =
  match arg with
  | Mem_arg (Rbp, offset) -> Int.max 0 (-offset)
                      (* ignore positive offset since stack grows downward *)
  | Lbl_arg _             -> 0
  | Imm_arg _             -> 0
  | Reg_arg _             -> 0
  | Mem_arg (_, _)        -> 0
;;

let _get_max_rbp_offset_instr (instr : 'a instr) : int =
  match instr with
  | Label _ -> 0
  | Load (arg, _) -> _get_max_rbp_offset_arg arg
  | Store (_, _, _) -> 0
  | Push _ -> 0
  | Pop _ -> 0
  | Binop (_, _, arg) -> _get_max_rbp_offset_arg arg
  | Cmp (arg, _) -> _get_max_rbp_offset_arg arg
  | Jmp _ -> 0
  | JmpC _ -> 0
  | SetC _ -> 0
  | Call_reg _ -> 0
  | Call_lbl _ -> 0
  | Ret ->  0
;;

(* NOTE 
 * - ignore push/pop since that changes rsp dynamically.
 * - look for negative offset only since stack grows downward,
 *   BUT RETURN POSITIVE offset for convenience. *)
let _get_max_rbp_offset_instrs (instrs : 'a instr list) : int =
  List.fold_left
    (fun max instr ->
       Int.max max (_get_max_rbp_offset_instr instr))
    0 instrs
;;

(* Yeah yeah internal modules; but bootstrapping takes priority *)
type spill_context =
  { next_slot      : int (* we could cache this in func, but KISS *)
  ; slot_map       : (Temp.t, int) Map.t (* keys ∈ [temps_to_spill] *)
  ; rev_instrs     : Temp.t instr list
  ; temps_to_spill : Temp.t Set.t
  ; rax_temp       : Temp.t
  }

let _spill_ctx_get_or_alloc_slot_offset (ctx : spill_context) (temp : Temp.t)
    : (spill_context * int) =
  let slot_to_offset (slot : int) : int =
    -slot * Constants.word_size
  in
  match Map.get temp ctx.slot_map with
  | Some slot -> (ctx, slot_to_offset slot)
  | None ->
    let slot = ctx.next_slot in
    let ctx = { ctx with next_slot = ctx.next_slot + 1;
                         slot_map  = Map.add temp slot ctx.slot_map }
    in (ctx, slot_to_offset slot)
;;

let _spill_ctx_add_instr (ctx : spill_context) (instr : Temp.t instr)
  : spill_context =
  { ctx with rev_instrs = instr::ctx.rev_instrs }
;;

let _spill_temp_read (ctx : spill_context) (temp : Temp.t) : spill_context =
  if Set.mem temp ctx.temps_to_spill
  then
    let ctx, offset = _spill_ctx_get_or_alloc_slot_offset ctx temp in
    let mem_arg = Mem_arg (Rbp, offset) in
    _spill_ctx_add_instr ctx (Load (mem_arg, Greg temp))
  else ctx
;;

let _spill_temp_write (ctx : spill_context) (temp : Temp.t) : spill_context =
  if Set.mem temp ctx.temps_to_spill
  then
    let ctx, offset = _spill_ctx_get_or_alloc_slot_offset ctx temp in
    _spill_ctx_add_instr ctx (Store (Greg temp, Rbp, offset))
  else ctx
;;

(* Calculate available stack slot and spill arguments if needed
 * (think of them as being written at entry). *)
let _spill_ctx_init temp_func (temps_to_spill : Temp.t Set.t) : spill_context =
  let max_rbp_offset = _get_max_rbp_offset_instrs temp_func.instrs in
  let max_slot = Int.ceil_div max_rbp_offset Constants.word_size in
  let ctx = { next_slot  = max_slot + 1
            ; slot_map   = Map.empty Temp.compare 
            ; rev_instrs = []
            ; temps_to_spill
            ; rax_temp   = temp_func.rax
            } in
  let args_to_spill = List.fold_right Set.remove temp_func.args temps_to_spill in
  Set.fold _spill_temp_write ctx args_to_spill (* order doesn't matter *)
;;

(* For any temp to spill
 * - always load from stack before a read
 * - always store to stack after a write
 * These break up the live-interval of spilled temps into smaller chunks. 
 *
 * NOTE assigning new names to different intervals might help reg-alloc,
 * but KISS for now. *)
let _spill_instr (ctx : spill_context) (instr : Temp.t instr) : spill_context =
  (* ASSUME instruction goes like read/compute/write *)
  let reads, writes = _get_reads_and_writes_temp_instr ctx.rax_temp instr in
  let ctx = List.fold_left _spill_temp_read ctx reads in
  let ctx = _spill_ctx_add_instr ctx instr in
  List.fold_left _spill_temp_write ctx writes
;;

let spill_temps temp_func temps_to_spill =
  let ctx = _spill_ctx_init temp_func temps_to_spill in
  let ctx = List.fold_left _spill_instr ctx temp_func.instrs in
  { temp_func with instrs = List.rev ctx.rev_instrs }
;;


let _physical_reg_to_int (pr : physical_reg) : int =
  match pr with
  | Rax -> 0
  | Rbx -> 1
  | Rsi -> 2
  | Rdi -> 3
  | Rdx -> 4
  | Rcx -> 5
  | R8  -> 6
  | R9  -> 7
  | R10 -> 8
  | R11 -> 9
  | R12 -> 10
  | R13 -> 11
  | R14 -> 12
  | R15 -> 13
;;

let _compare_physical_reg r1 r2 =
  Int.compare (_physical_reg_to_int r1) (_physical_reg_to_int r2)
;;

let _physical_reg_list_to_set (prs : physical_reg list) : physical_reg Set.t =
  List.fold_right Set.add prs (Set.empty _compare_physical_reg)
;;

let caller_saved_physical_regs =
  _physical_reg_list_to_set [Rdi; Rsi; Rdx; Rcx; R8; R9; Rax; R11; R12]
;;

let callee_saved_physical_regs =
  _physical_reg_list_to_set [Rbx; R12; R13; R14; R15]
;;

let rax_physical_reg = Rax
;;


let _get_callee_saved_prs (pr_assignment : (Temp.t, physical_reg) Map.t)
  : physical_reg Set.t =
  Map.fold
    (fun pr callee_saved ->
       if Set.mem pr callee_saved_physical_regs
       then Set.add pr callee_saved
       else callee_saved)
    pr_assignment
    (Set.empty _compare_physical_reg)
;;

let _temp_to_pr (pr_assignment : (Temp.t, physical_reg) Map.t) (temp : Temp.t)
  : physical_reg =
  match Map.get temp pr_assignment with
  | None -> failwith "[X86._temp_to_pr] no physical register for temp"
  | Some pr -> pr
;;

let _reg_temp_to_pr
    (pr_assignment : (Temp.t, physical_reg) Map.t) (reg : Temp.t reg)
  : physical_reg reg =
  match reg with
  | Rsp -> Rsp
  | Rbp -> Rbp
  | Greg temp -> Greg (_temp_to_pr pr_assignment temp)
;;

let _arg_temp_to_pr
    (pr_assignment : (Temp.t, physical_reg) Map.t) ( arg: Temp.t arg)
  : physical_reg  arg=
  match arg with
  | Lbl_arg label -> Lbl_arg label
  | Imm_arg n     -> Imm_arg n
  | Reg_arg reg   -> Reg_arg (_reg_temp_to_pr pr_assignment reg)
  | Mem_arg (reg, offset) ->
    Mem_arg (_reg_temp_to_pr pr_assignment reg, offset)
;;

let _instr_temp_to_pr
    (pr_assignment : (Temp.t, physical_reg) Map.t) (instr : Temp.t instr)
  : physical_reg instr =
  match instr with
  | Label label -> Label label
  | Load (arg, dst_reg) ->
    let arg = _arg_temp_to_pr pr_assignment arg in
    let dst_reg = _reg_temp_to_pr pr_assignment dst_reg in
    Load (arg, dst_reg)

  | Store (src_reg, dst_addr_reg, offset) ->
    let src_reg = _reg_temp_to_pr pr_assignment src_reg in
    let dst_addr_reg = _reg_temp_to_pr pr_assignment dst_addr_reg in
    Store (src_reg, dst_addr_reg, offset)

  | Push reg ->
    Push (_reg_temp_to_pr pr_assignment reg)

  | Pop reg ->
    Pop (_reg_temp_to_pr pr_assignment reg)

  | Binop (binop, reg, arg) ->
    let arg = _arg_temp_to_pr pr_assignment arg in
    let reg = _reg_temp_to_pr pr_assignment reg in
    Binop (binop, reg, arg)

  | Cmp (arg, temp) ->
    let arg = _arg_temp_to_pr pr_assignment arg in
    let pr = _temp_to_pr pr_assignment temp in
    Cmp (arg, pr)

  | Jmp label -> Jmp label

  | JmpC (cond, label) -> JmpC (cond, label)

  | SetC (cond, temp) ->
    SetC (cond, _temp_to_pr pr_assignment temp)

  | Call_reg temp -> 
    Call_reg (_temp_to_pr pr_assignment temp)

  | Call_lbl label -> Call_lbl label
  | Ret -> Ret
;;

(*      ......
 * caller stack frame
 * ------------------
 * | return address |
 * ------------------ <- RBP + 1 * word_size = old_rsp
 * |    saved rbp   | 
 * ------------------ <- RBP + 0 * word_size 
 *  func stack frame
 *      ......        *)
let temp_func_to_func temp_func pr_assignment =
  let callee_saved = Set.to_list (_get_callee_saved_prs pr_assignment) in
  (* TODO stack alignment? *)
  let max_rbp_offset = _get_max_rbp_offset_instrs temp_func.instrs in
  let spill_callee_saved =
    List.map (fun pr -> Push (Greg pr)) callee_saved
  in
  let restore_callee_saved =
    List.map (fun pr -> Pop (Greg pr)) (List.rev callee_saved)
  in
  let prologue =
    List.append
      [ 
        Push Rbp;
        Load (Reg_arg Rsp, Rbp);
        Binop (Sub, Rsp, Imm_arg max_rbp_offset);
      ] 
      spill_callee_saved
  in
  let epilogue_label = Label.to_epilogue temp_func.entry in
  let epilogue =
    List.append
    ((Label epilogue_label)::restore_callee_saved)
    [
      Load (Reg_arg Rbp, Rsp);
      Pop Rbp;
      Ret;
    ]
  in
  let pr_instrs = List.map (_instr_temp_to_pr pr_assignment) temp_func.instrs in
  let final_instrs = List.append prologue (List.append pr_instrs epilogue) in
  { entry  = temp_func.entry;
    instrs = final_instrs }
;;


let _physical_reg_to_str (r : physical_reg) : string =
  match r with
  | Rax -> "RAX"
  | Rbx -> "RBX"
  | Rsi -> "RSI"
  | Rdi -> "RDI"
  | Rdx -> "RDX"
  | Rcx -> "RCX"
  | R8  -> "R8"
  | R9  -> "R9"
  | R10 -> "R10"
  | R11 -> "R11"
  | R12 -> "R12"
  | R13 -> "R13"
  | R14 -> "R14"
  | R15 -> "R15"
;;

let _reg_to_str (reg : 'a reg) (gr_to_str : 'a -> string) : string =
  match reg with
  | Rsp -> "RSP"
  | Rbp -> "RBP"
  | Greg gr -> gr_to_str gr
;;


let _reg_offset_to_str
    (reg : 'a reg) (offset : int) (gr_to_str : 'a -> string)
  : string =
  let reg_str = _reg_to_str reg gr_to_str in
  let op_str, offset =
    if offset < 0
    then " - ", -offset
    else " + ", offset
  in
  let offset_str = Int.to_string offset in
  String.join_with ["["; reg_str; op_str; offset_str ; "]"] ""
;;

let _arg_to_str (arg : 'a arg) (gr_to_str : 'a -> string) : string =
  match arg with
  | Lbl_arg label        -> Label.to_string label
  | Imm_arg n            -> Int.to_string n
  | Reg_arg reg           -> _reg_to_str reg gr_to_str
  | Mem_arg (reg, offset) -> _reg_offset_to_str reg offset gr_to_str
;;

let _cond_to_suffix (cond : cond) : string =
  match cond with
  | Eq -> "e"
  | Lt -> "l"
;;

let _binop_to_str (binop : binop) : string =
  match binop with
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "imul" (* 'i' for signed integer multiplication *)
;;

let _instr_to_str (instr : 'a instr) (gr_to_str : 'a -> string) : string =
  let add_tab s = String.append "\t" s in
  match instr with
  | Label label -> String.append (Label.to_string label) ":"

  | Load (arg, dst_reg) ->
    let arg_str = _arg_to_str arg gr_to_str in
    let dst_str = _reg_to_str dst_reg gr_to_str in
    let instr_str = String.join_with ["mov "; dst_str; ", "; arg_str;] "" in
    add_tab instr_str

  | Store (src_reg, dst_addr_reg, offset) ->
    let src_str = _reg_to_str src_reg gr_to_str in
    let dst_str = _reg_offset_to_str dst_addr_reg offset gr_to_str in
    let instr_str = String.join_with ["mov "; dst_str; ", "; src_str;] "" in
    add_tab instr_str

  | Push reg ->
    let reg_str = _reg_to_str reg gr_to_str in
    let instr_str = String.join_with ["push"; reg_str] " " in
    add_tab instr_str

  | Pop reg ->
    let reg_str = _reg_to_str reg gr_to_str in
    let instr_str = String.join_with ["pop"; reg_str] " " in
    add_tab instr_str

  | Binop (binop, reg, arg) ->
    let arg_str = _arg_to_str arg gr_to_str in
    let reg_str = _reg_to_str reg gr_to_str in
    let binop_str = _binop_to_str binop in
    let instr_str =
      String.join_with [binop_str; " "; reg_str; ", "; arg_str;] "" in
    add_tab instr_str

  | Cmp (arg, pr) ->
    let arg_str = _arg_to_str arg gr_to_str in
    let pr_str = _physical_reg_to_str pr in
    let instr_str = String.join_with ["cmp "; arg_str; ", "; pr_str;] "" in
    add_tab instr_str

  | Jmp label ->
    let instr_str = String.append "jmp " (Label.to_string label)in
    add_tab instr_str

  | JmpC (cond, label) ->
    let suffix = _cond_to_suffix cond in
    let label_str = Label.to_string label in
    let instr_str = String.join_with ["j"; suffix; " "; label_str] "" in
    add_tab instr_str

  | SetC (cond, gr) -> 
    let suffix = _cond_to_suffix cond in
    let gr_str = gr_to_str gr in
    let instr_str = String.join_with ["set"; suffix; " "; gr_str] "" in
    add_tab instr_str

  | Call_reg gr -> 
    let gr_str = gr_to_str gr in
    let instr_str = String.join_with ["call"; " "; gr_str] "" in
    add_tab instr_str

  | Call_lbl label -> 
    let label_str = Label.to_string label in
    let instr_str = String.join_with ["call"; " "; label_str] "" in
    add_tab instr_str

  | Ret -> add_tab "ret"
;;

let _instrs_to_str (instrs : 'a instr list) (gr_to_str : 'a -> string)
  : string =
  let instr_strs = List.map (fun instr -> _instr_to_str instr gr_to_str) instrs in
  String.join_with instr_strs "\n"
;;

let _func_to_str (func : func) : string =
  let all_instrs = (Label func.entry)::func.instrs in
  _instrs_to_str all_instrs _physical_reg_to_str
;;


let _add_label_if_native (labels : Label.t Set.t) (label : Label.t)
  : Label.t Set.t =
  if Label.is_native label 
  then Set.add label labels
  else labels
;;

let _add_native_labels_in_arg (labels : Label.t Set.t) (arg : 'a arg)
  : Label.t Set.t =
  match arg with
  | Imm_arg _ | Reg_arg _ | Mem_arg _ -> labels
  | Lbl_arg label ->
    _add_label_if_native labels label
;;

let _add_external_native_labels_in_instr
    (labels : Label.t Set.t) (instr : 'a instr)
  : Label.t Set.t =
  match instr with
  | Label _           -> labels
  | Load (arg, _)     -> _add_native_labels_in_arg labels arg
  | Binop (_, _, arg) -> _add_native_labels_in_arg labels arg
  | Cmp (arg, _)      -> _add_native_labels_in_arg labels arg
  | Call_lbl label    -> _add_label_if_native labels label

  | Store _ | Push _ | Pop _ | Jmp _ | JmpC _ | SetC _ | Call_reg _ | Ret -> 
    labels
;;

let _add_external_native_labels_in_func
    (labels : Label.t Set.t) (func : func) : Label.t Set.t =
  List.fold_left _add_external_native_labels_in_instr labels func.instrs
;;

let _find_external_native_labels_in_prog (prog : prog) : Label.t Set.t =
  let labels = Set.empty Label.compare in
  let labels =
    List.fold_left _add_external_native_labels_in_func labels prog.funcs
  in
  _add_external_native_labels_in_func labels prog.main
;;

let _get_prog_metadata (prog : prog) : string =
  let external_native_label_decls = 
    List.map
      (fun label -> String.append "extern " (Label.to_string label))
      (Set.to_list (_find_external_native_labels_in_prog prog))
  in
  let metadata_lines = "section .text"::external_native_label_decls in
  String.join_with metadata_lines "\n"
;;

(* "ur" stands for usable_register *)
let prog_to_str (prog : prog) : string =
  let metadata_str = _get_prog_metadata prog in
  let all_funcs = List.append prog.funcs [prog.main] in
  let func_strs = List.map _func_to_str all_funcs in
  let funcs_str = String.join_with func_strs "\n\n" in
  String.join_with [metadata_str; funcs_str] "\n\n"
;;
