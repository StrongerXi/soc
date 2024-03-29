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

let _compare_physical_reg r1 r2 =
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
  in
  Int.compare (_physical_reg_to_int r1) (_physical_reg_to_int r2)
;;

let _physical_reg_list_to_set (prs : physical_reg list) : physical_reg Set.t =
  Set.add_list prs (Set.empty _compare_physical_reg)
;;

let _ordered_argument_physical_regs =
  [Rdi; Rsi; Rdx; Rcx; R8; R9]
;;

let _caller_saved_physical_regs =
  _physical_reg_list_to_set [Rdi; Rsi; Rdx; Rcx; R8; R9; Rax; R10; R11]
;;

let _callee_saved_physical_regs =
  _physical_reg_list_to_set [Rbx; R12; R13; R14; R15]
;;

let assignable_regs =
  Set.union _caller_saved_physical_regs _callee_saved_physical_regs
;;


type 'a reg =
  | Rsp        (* not sure how allowing this in reg-alloc would affect things *)
  | Rbp        (* we need rbp for spilling *)
  | Greg of 'a (* general purpose registers *)

type 'a arg =
  | Lbl_arg of Label.t      (* label, e.g., use function label as data *)
  | Imm_arg of int          (* immediate, i.e., constants*)
  | Reg_arg of 'a reg       (* load from [reg] *)
  | Mem_arg of 'a reg * int (* load from address [reg + offset] *)

type cond =
  | Eq
  | Lt

type binop =
  | Add
  | Sub
  | Mul

type 'a call_target =
  | Reg of 'a
  | Lbl of Label.t

(* NOTE 
 * 0. I'm not being truthful to the X86 ISA here. I really took a subset of it
 *    for simplicity, and since we don't need the other fancy instrs for now.
 * 1. Many instructions don't support 2 memory accesses, I made that explicit.
 *)
type 'a instr = 
  | Label of Label.t
  | Load of 'a arg * 'a reg
      (* Load (arg, reg) --> reg := arg *)
  | Store of 'a reg * 'a reg * int
      (* Store (sreg, dreg, offset) --> *[dreg + offset] := sreg *)
  | Push of 'a reg
  | Pop of 'a reg
  | IDiv of 'a reg
      (* [IDiv r] does signed division for [RDX:RAX] over [r].
      * The quotient is stoed to [RAX], remainder to [RDX] *)
  | Binop of binop * 'a reg * 'a arg
      (* Binop (op, reg, arg) --> reg := op gr arg *)
  | Cmp of 'a arg * 'a reg
  | Jmp of Label.t
  | JmpC of cond * Label.t
      (* Jump to label if [cond] is satisfied. *)
  | Call of 'a call_target * 'a list
      (* Target and arguments that are passed in registers in no specific order.
       * The arguments are used for debugging or liveness analysis purposes *)
  | Ret
      (* Return control flow to caller site *)


type func =
  { entry  : Label.t
  ; instrs : physical_reg instr list (* doesn't start with [entry] label *)
  }

type 'a prog = 
  { funcs : 'a list
  ; main : 'a
  }

(* special purpose temps that will be pre-colored to specific registers *)
type sp_temps =
  { rax              : Temp.t
  ; rdx              : Temp.t
  (* TODO consider using a pair list here *)
  ; ordered_arg_regs : (Temp.t * physical_reg) list 
      (* For each argument passed in register.
       * NOTE Original Lir func might not have this many args, but calls to
       * other func might need more args. Excessive temps are simply ignored. *)
  ; caller_saved     : (Temp.t, physical_reg) Map.t
  }

(* ENSURES: Same temps will be used for same regs *)
let _init_sp_temps (init_temp_man : Temp.manager) : (Temp.manager * sp_temps) =

  let alloc_temp_for_prs (temp_man : Temp.manager) (prs : physical_reg Set.t)
    : (Temp.manager * (physical_reg, Temp.t) Map.t) =
    Set.fold
      (fun (temp_man, pr_map) pr ->
         let temp_man, temp = Temp.gen temp_man in
         let pr_map = Map.add pr temp pr_map in
         (temp_man, pr_map))
      (temp_man, Map.empty _compare_physical_reg)
      prs
  in

  let get_temp (pr_map : (physical_reg, Temp.t) Map.t) (pr : physical_reg)
    : Temp.t =
    match Map.get pr pr_map with
    | None -> failwith "[X86._init_sp_temps] unbound physical register"
    | Some temp -> temp
  in

  let sp_prs = Set.add_list
      (Rax::Rdx::_ordered_argument_physical_regs)
      (Set.empty _compare_physical_reg)
  in
  let sp_prs = Set.union sp_prs _caller_saved_physical_regs in
  let temp_man, pr_map = alloc_temp_for_prs init_temp_man sp_prs in
  let sp_temps = { rax = get_temp pr_map Rax
                 ; rdx = get_temp pr_map Rdx
                 ; ordered_arg_regs =
                     List.map
                       (fun pr -> (get_temp pr_map pr, pr))
                       _ordered_argument_physical_regs
                 ; caller_saved =
                     Set.fold
                       (fun map pr -> Map.add (get_temp pr_map pr) pr map)
                       (Temp.empty_map ())
                       _caller_saved_physical_regs
                 }
  in (temp_man, sp_temps)

let _get_sp_temps_coloring sp_temps : (Temp.t, physical_reg) Map.t =
  let pre_color = sp_temps.caller_saved in
  let temp_reg_pairs =
    (sp_temps.rax, Rax)::
    (sp_temps.rdx, Rdx)::
    (sp_temps.ordered_arg_regs)
  in
  Map.add_pairs temp_reg_pairs pre_color
;;


(* With some annotations to help reg-alloc. *)
type temp_func = 
  { entry    : Label.t
  ; instrs   : Temp.t instr list  (* doesn't start with [entry] label *)
  ; sp_temps : sp_temps
  ; temp_manager : Temp.manager (* for generating fresh temps *)
  }


let _map_grs_in_reg (f : 'a -> 'b) (reg : 'a reg) : 'b reg =
  match reg with
  | Rsp -> Rsp
  | Rbp -> Rbp
  | Greg gr -> Greg (f gr)
;;

let _map_grs_in_arg (f : 'a -> 'b) (arg : 'a arg) : 'b arg =
  match arg with
  | Lbl_arg label    -> Lbl_arg label
  | Imm_arg n        -> Imm_arg n
  | Reg_arg reg      -> Reg_arg (_map_grs_in_reg f reg)
  | Mem_arg (reg, n) -> Mem_arg (_map_grs_in_reg f reg, n)
;;

let _map_grs_in_call_target (f : 'a -> 'b) (target : 'a call_target)
  : 'b call_target =
  match target with
  | Reg gr  -> Reg (f gr)
  | Lbl label -> Lbl label
;;

let _map_grs_in_instr (f : 'a -> 'b) (instr : 'a instr) : ('b instr) =
  match instr with
  | Label label        -> Label label
  | Jmp label          -> Jmp label
  | JmpC (cond, label) -> JmpC (cond, label)

  | Push reg -> Push (_map_grs_in_reg f reg)
  | Pop reg  -> Pop (_map_grs_in_reg f reg)
  | IDiv reg -> IDiv (_map_grs_in_reg f reg)

  | Load (arg, dst_reg) ->
    let arg = _map_grs_in_arg f arg in
    let dst_reg = _map_grs_in_reg f dst_reg in
    Load (arg, dst_reg)

  | Store (src_reg, dst_addr_reg, offset) ->
    let src_reg = _map_grs_in_reg f src_reg in
    let dst_addr_reg = _map_grs_in_reg f dst_addr_reg in
    Store (src_reg, dst_addr_reg, offset)

  | Binop (op, reg, arg) ->
    let arg = _map_grs_in_arg f arg in
    let reg = _map_grs_in_reg f reg in
    Binop (op, reg, arg)

  | Cmp (arg, reg) ->
    let arg = _map_grs_in_arg f arg in
    Cmp (arg, _map_grs_in_reg f reg)

  | Call (target, reg_arg_grs) ->
    let target = _map_grs_in_call_target f target in
    let reg_arg_grs = List.map f reg_arg_grs in
    Call (target, reg_arg_grs)

  | Ret -> Ret
;;


(* context for translation from [Lir.func] to [temp_func] *)
type context =
  { func_label : Label.t
  ; sp_temps   : sp_temps
  (* following are "mutable" *)
  ; temp_manager  : Temp.manager
  ; label_manager : Label.manager
  ; rev_instrs    : Temp.t instr list
  }

let _init_ctx
    (func_label : Label.t)
    (temp_manager : Temp.manager) (label_manager : Label.manager)
  : context =
  let temp_manager, sp_temps = _init_sp_temps temp_manager in
  { func_label; sp_temps; temp_manager; label_manager; rev_instrs = [] }
;;

let _ctx_get_rax_reg (ctx : context) : Temp.t reg =
  Greg ctx.sp_temps.rax
;;

let _ctx_get_rdx_reg (ctx : context) : Temp.t reg =
  Greg ctx.sp_temps.rdx
;;

let _ctx_get_ordered_arg_regs (ctx : context) : Temp.t list =
  List.map (fun (temp, _) -> temp) ctx.sp_temps.ordered_arg_regs
;;

let _ctx_add_instr (ctx : context) (instr : Temp.t instr) : context =
  let rev_instrs = instr::ctx.rev_instrs in
  { ctx with rev_instrs }
;;

(* first instruction in [instr] is added first *)
let _ctx_add_instrs (ctx : context) (instrs : Temp.t instr list) : context =
  let rev_instrs =
    List.fold_left (* add [instrs] in order *)
      (fun rev_instrs instr -> instr::rev_instrs)
      ctx.rev_instrs instrs
  in
  { ctx with rev_instrs }
;;

let _ctx_get_instrs (ctx : context) : Temp.t instr list =
  List.rev ctx.rev_instrs
;;

let _ctx_gen_temp_reg (ctx : context) : (context * Temp.t reg)  =
  let temp_manager, temp = Temp.gen ctx.temp_manager in
  ({ ctx with temp_manager }, Greg temp)
;;

let _ctx_gen_label (ctx : context) (name : string) : (context * Label.t) =
  let label_manager, label = Label.gen ctx.label_manager name in
  ({ ctx with label_manager }, label)
;;

let _ctx_get_epilogue_label (ctx : context) : Label.t =
  Label.to_epilogue ctx.func_label
;;

(* Load all args into temps (from regs or stack).
 * This ensures that no reg-passed arg will be modified.
 * Must synch with [_emit_prepare_x86_call_args] *)
let _emit_load_args_into_temps (ctx : context) (ordered_args : Temp.t list)
  : context =
  let load_extra_args ctx extra_arg_temps : context =
    List.fold_left
      (fun (ctx, offset_bytes) arg_temp ->
         let mem_arg = Mem_arg (Rbp, offset_bytes) in
         let instr = Load (mem_arg, Greg arg_temp) in
         let ctx = _ctx_add_instr ctx instr in
         (ctx, offset_bytes + Runtime.word_size))
      (ctx, 2 * Runtime.word_size) extra_arg_temps
    |> (fun (ctx, _) -> ctx)
  in
  let rec go ctx (arg_temps : Temp.t list) (arg_regs : Temp.t list) : context =
    match arg_temps, arg_regs with
    | [], _ -> ctx
    | _, [] -> load_extra_args ctx arg_temps
    | arg_temp::rest_arg_temps, arg_reg::rest_arg_prs ->
      let instr = Load (Reg_arg (Greg arg_reg), Greg arg_temp) in
      let ctx = _ctx_add_instr ctx instr in
      go ctx rest_arg_temps rest_arg_prs
  in
  let ordered_arg_regs = _ctx_get_ordered_arg_regs ctx in
  go ctx ordered_args ordered_arg_regs
;;

let rec _emit_lir_expr (ctx : context) (e : Lir.expr) (dst_reg: Temp.t reg)
  : context =
  match e with
  | Imm n -> 
    let instr = Load (Imm_arg n, dst_reg) in
    _ctx_add_instr ctx instr

  | Tmp temp -> (* XXX generate to [arg] would save 1 instr *)
    let instr = Load (Reg_arg (Greg temp), dst_reg) in
    _ctx_add_instr ctx instr

  | Op (op, lhs_e, rhs_e) ->
    _emit_lir_op ctx op lhs_e rhs_e dst_reg

  | Call (label_temp, arg_es) ->
    _emit_generic_call ctx arg_es (Reg label_temp) dst_reg

  | NativeCall (func_label, arg_es) ->
    _emit_generic_call ctx arg_es (Lbl func_label) dst_reg

  | Mem_alloc nbytes ->
    _emit_generic_call ctx [Imm nbytes] (Lbl Runtime.mem_alloc_label) dst_reg

(* abstract over the call target *)
and _emit_generic_call (ctx : context)
    (arg_es : Lir.expr list) (target : Temp.t call_target) (dst_reg : Temp.t reg)
  : context =
    let ctx, arg_temps, arg_bytes = _emit_prepare_x86_call_args ctx arg_es in
    _ctx_add_instrs ctx
      [ Call (target, arg_temps);
        Binop (Add, Rsp, Imm_arg arg_bytes);
        Load (Reg_arg (_ctx_get_rax_reg ctx), dst_reg) ]

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
 *      ......        
 * NOTE stack alignment will be enforced.
 * Return 
 * - Temps which holds the register arguments in order.
 *   Args spilled onto stack are ignored, since they don't need to be in any
 *   register during the call instruction -- they are saved on stack.
 * - How many bytes the aligned extra args took up on stack. *)
and _emit_prepare_x86_call_args (init_ctx : context)
    (init_arg_es : Lir.expr list)
  : (context * Temp.t list * int) =
  (* last argument is pushed first, so 1st arg is closest to frame base *)
  let _emit_push_onto_stack ctx arg_es =
    let ctx =
      List.fold_right (* TODO fold_left instead? *)
        (fun arg_e ctx ->
           let ctx, arg_v_reg = _ctx_gen_temp_reg ctx in
           let ctx = _emit_lir_expr ctx arg_e arg_v_reg in
           _ctx_add_instr ctx (Push arg_v_reg))
        arg_es ctx
    in
    let extra_arg_offset = (List.length arg_es) * Runtime.word_size in
    let align_offset =
      Int.offset_to_align extra_arg_offset Runtime.stack_alignment
    in (* sometimes redundant, but KISS for now *)
    let align_rsp_i = Binop (Sub, Rsp, Imm_arg align_offset) in
    let ctx = _ctx_add_instr ctx align_rsp_i in
    (ctx, extra_arg_offset + align_offset)
  in

  (* NOTE Hopefully coalescing or other optimization could eliminate some
   * redundant moves here. Also can consider evaluating arg-expr with most
   * nested calls first, to avoid arg-temps living across calls *)
  let move_temps_ret_dsts ctx (src_to_dst_pairs : (Temp.t reg * Temp.t) list)
    : (context * Temp.t list) =
    List.fold_left
      (fun (ctx, dsts) (src_reg, dst_temp) ->
         let instr = Load (Reg_arg src_reg, Greg dst_temp) in
         let ctx = _ctx_add_instr ctx instr in
         (ctx, dst_temp::dsts))
    (ctx, []) src_to_dst_pairs
  in

  let rec go ctx arg_es
      (ordered_arg_regs : Temp.t list)
      (arg_v_temp_pairs : (Temp.t reg * Temp.t) list) =
    match arg_es, ordered_arg_regs  with
    | [], _ ->
      let ctx, arg_temps  = move_temps_ret_dsts ctx arg_v_temp_pairs in
      (ctx, arg_temps, 0) (* 0 is always aligned *)

    | _, [] -> 
      let ctx, arg_bytes = _emit_push_onto_stack ctx arg_es in
      let ctx, arg_temps = move_temps_ret_dsts ctx arg_v_temp_pairs in
      (ctx, arg_temps, arg_bytes)

    | arg_e::rest_arg_es, arg_temp::rest_arg_temps ->
      let ctx, arv_v_reg = _ctx_gen_temp_reg ctx in
      let ctx = _emit_lir_expr ctx arg_e arv_v_reg in
      let arg_temp_pairs = (arv_v_reg, arg_temp)::arg_v_temp_pairs in
      go ctx rest_arg_es rest_arg_temps arg_temp_pairs
  in

  let ordered_arg_regs = _ctx_get_ordered_arg_regs init_ctx in
  go init_ctx init_arg_es ordered_arg_regs  []

and _emit_lir_op (ctx : context)
    (op : Lir.op) (lhs_e : Lir.expr) (rhs_e : Lir.expr) (dst_reg : Temp.t reg)
  : context =
  let ctx, rhs_reg = _ctx_gen_temp_reg ctx in
  let ctx = _emit_lir_expr ctx lhs_e dst_reg in
  let ctx = _emit_lir_expr ctx rhs_e rhs_reg in
  match op with
  | Add ->
    _ctx_add_instr ctx (Binop (Add, dst_reg, Reg_arg rhs_reg))
  | Sub ->
    _ctx_add_instr ctx (Binop (Sub, dst_reg, Reg_arg rhs_reg))
  | Mul -> 
    _ctx_add_instr ctx (Binop (Mul, dst_reg, Reg_arg rhs_reg))
  | Div ->
    let rax_reg = _ctx_get_rax_reg ctx in
    _ctx_add_instrs ctx
      [ Load (Imm_arg 0, _ctx_get_rdx_reg ctx);
        Load (Reg_arg dst_reg, rax_reg);
        IDiv rhs_reg;
        Load (Reg_arg rax_reg, dst_reg); ]
;;

let _emit_comparison
    (ctx : context) (lhs_e : Lir.expr) (rhs_e : Lir.expr)
    : context =
  let ctx, lhs_reg = _ctx_gen_temp_reg ctx in
  let ctx, rhs_reg = _ctx_gen_temp_reg ctx in
  let ctx = _emit_lir_expr ctx lhs_e lhs_reg in
  let ctx = _emit_lir_expr ctx rhs_e rhs_reg in
  let instr = Cmp (Reg_arg lhs_reg, rhs_reg) in
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

let _emit_lir_instr (ctx : context) (lir_instr : Lir.instr) : context =
  (* TODO this could generate seriously inefficient code.
   * To optimize, e.g., consider emit certain expr into arg instead of reg *)
  match lir_instr with
  | Label label ->
    _ctx_add_instr ctx (Label label)

  | Load (e, dst_temp) ->
    _emit_lir_expr ctx e (Greg dst_temp)

  | LoadMem (src_addr_e, dst_temp) ->
    let ctx = _emit_lir_expr ctx src_addr_e (Greg dst_temp) in
    let instr = Load (Mem_arg(Greg dst_temp, 0), Greg dst_temp) in
    _ctx_add_instr ctx instr

  | Store (e, dst_addr_e) ->
    let ctx, e_reg = _ctx_gen_temp_reg ctx in
    let ctx, dst_addr_reg = _ctx_gen_temp_reg ctx in
    let ctx = _emit_lir_expr ctx e e_reg in
    let ctx = _emit_lir_expr ctx dst_addr_e dst_addr_reg in
    let instr = Store (e_reg, dst_addr_reg, 0) in
    _ctx_add_instr ctx instr

  | Store_label (label, dst_addr_e) ->
    let ctx, dst_addr_reg = _ctx_gen_temp_reg ctx in
    let ctx = _emit_lir_expr ctx dst_addr_e dst_addr_reg in
    let ctx, label_reg = _ctx_gen_temp_reg ctx in
    _ctx_add_instrs ctx
      [ Load (Lbl_arg(label), label_reg);
        Store (label_reg, dst_addr_reg, 0); ]

  | Jump (lir_cond, target_label) ->
    _emit_lir_jump ctx lir_cond target_label

  | Ret e ->
    let ctx = _emit_lir_expr ctx e (_ctx_get_rax_reg ctx) in
    let epilogue_label = _ctx_get_epilogue_label ctx in
    _ctx_add_instr ctx (Jmp epilogue_label)
;;

let _emit_lir_instrs (ctx : context) (lir_instrs : Lir.instr list) : context =
  List.fold_left _emit_lir_instr ctx lir_instrs
;;

let _from_lir_func_impl
    (label_manager : Label.manager) (temp_manager : Temp.manager)
    (entry : Label.t) (ordered_args : Temp.t list) (body : Lir.instr list)
  : (temp_func * Label.manager) =
  let ctx = _init_ctx entry temp_manager label_manager in
  let ctx = _emit_load_args_into_temps ctx ordered_args in
  let ctx = _emit_lir_instrs ctx body in
  let func = { entry
             ; sp_temps     = ctx.sp_temps
             ; instrs       = _ctx_get_instrs ctx
             ; temp_manager = ctx.temp_manager
             }
  in (func, ctx.label_manager)
;;

let _from_lir_func (label_manager : Label.manager) (lf : Lir.func)
  : (temp_func * Label.manager) =
  _from_lir_func_impl
    label_manager lf.temp_manager lf.name lf.ordered_args lf.body
;;

let _from_lir_funcs (label_manager : Label.manager) (lir_funcs : Lir.func list)
  : (temp_func list * Label.manager) =
  List.fold_left
    (fun (funcs, label_manager) lir_func ->
       let func, label_manager = _from_lir_func label_manager lir_func in
       (func::funcs, label_manager))
    ([], label_manager)
    lir_funcs
;;

let _from_lir_main_func
    (temp_manager : Temp.manager)
    (label_manager : Label.manager)
    (body_instrs : Lir.instr list)
  : temp_func =
  (* NOTE assume the entry function 
   * - is invoked by runtime with the designated label
   * - has no args 
   * MUST synch with C runtime *)
  let main_entry = Runtime.entry_label in
  _from_lir_func_impl label_manager temp_manager main_entry [] body_instrs
  |> (fun (temp_func, _) -> temp_func)
;;

let from_lir_prog (lir_prog : Lir.prog) =
  let temp_funcs, label_manager =
    _from_lir_funcs lir_prog.label_manager lir_prog.funcs
  in
  let temp_main =
    _from_lir_main_func lir_prog.temp_manager label_manager lir_prog.entry
  in
  { funcs = temp_funcs ; main = temp_main }
;;


let _add_temps_in_temp_reg (acc : Temp.t Set.t) (reg : Temp.t reg)
  : Temp.t Set.t =
  match reg with
  | Rsp | Rbp -> acc
  | Greg temp -> Set.add temp acc
;;

let _add_temps_in_temp_arg  (acc : Temp.t Set.t) (arg : Temp.t arg)
  : Temp.t Set.t =
  match arg with
  | Lbl_arg _        -> acc
  | Imm_arg _        -> acc
  | Reg_arg reg      -> _add_temps_in_temp_reg acc reg
  | Mem_arg (reg, _) -> _add_temps_in_temp_reg acc reg
;;

let _add_temps_in_call_target (acc : Temp.t Set.t) (target : Temp.t call_target)
  : Temp.t Set.t =
  match target with
  | Reg temp -> Set.add temp acc
  | Lbl _ -> acc
;;

let _get_reads_and_writes_temp_instr sp_temps (instr : Temp.t instr)
  : (Temp.t Set.t * Temp.t Set.t) =
  let reads, writes = (Temp.empty_set, Temp.empty_set) in
  match instr with
  | Label _          -> (reads, writes)
  | Jmp _            -> (reads, writes)
  | JmpC (_, _)      -> (reads, writes)

  | Push reg -> 
    let reads = _add_temps_in_temp_reg reads reg in
    (reads, writes)

  | Pop reg  ->
    let writes = _add_temps_in_temp_reg writes reg in
    (reads, writes)

  | IDiv reg  ->
    let reads = _add_temps_in_temp_reg reads reg in
    let reads = Set.add_list [sp_temps.rax; sp_temps.rdx] reads in
    let writes = Set.add_list [sp_temps.rax; sp_temps.rdx] writes in
    (reads, writes)

  | Load (arg, dst_reg) ->
    let reads = _add_temps_in_temp_arg reads arg in
    let writes = _add_temps_in_temp_reg writes dst_reg in
    (reads, writes)

    (* storing to memory, so no reg is written *)
  | Store (src_reg, dst_addr_reg, _) ->
    let reads = _add_temps_in_temp_reg reads src_reg in
    let reads = _add_temps_in_temp_reg reads dst_addr_reg in
    (reads, writes)

  | Binop (_, reg, arg) ->
    let reads = _add_temps_in_temp_reg reads reg in
    let reads = _add_temps_in_temp_arg reads arg in
    let writes = _add_temps_in_temp_reg writes reg in
    (reads, writes)

  | Cmp (arg, reg) ->
    let reads = _add_temps_in_temp_reg reads reg in 
    let reads = _add_temps_in_temp_arg reads arg in
    (reads, writes)

  (* Q: What about caller-saved registers?
   * A: We ignore all details about calling convention at this level (except
   *    for RAX, because our instr doesn't specify "dst_reg" of call).
   *    Basically we leave it to the register allocator to ensure that
   *    caller-saved registers are properly assigned or spilled *)
  | Call (target, reg_arg_temps) ->
    let reads = Set.add_list reg_arg_temps reads in
    let reads = _add_temps_in_call_target reads target in
    let writes = Set.add sp_temps.rax writes in
    let writes = Set.union (Map.get_key_set sp_temps.caller_saved) writes in
    (reads, writes)

  | Ret ->
    let reads = Set.add sp_temps.rax reads in
    (reads, writes)
;;

let _temp_instr_to_vasm (sp_temps : sp_temps) (instr : Temp.t instr) : Vasm.t =
  let reads, writes = _get_reads_and_writes_temp_instr sp_temps instr in
  let reads, writes = Set.to_list reads, Set.to_list writes in
  match instr with
  | Label label -> Vasm.mk_label label

  | Load _ | Store _ | Push _ | Pop _ | Binop _ | Cmp _ | IDiv _ | Call _ ->
    Vasm.mk_instr reads writes

  | Jmp target -> Vasm.mk_dir_jump reads writes target
  | JmpC (_, target) -> Vasm.mk_cond_jump reads writes target

  | Ret -> Vasm.mk_ret reads writes
;;

let temp_func_to_vasms (temp_func : temp_func) =
  let sp_temps = temp_func.sp_temps in
  let body_vasms = List.map (_temp_instr_to_vasm sp_temps) temp_func.instrs in
  let entry_label = Vasm.mk_label temp_func.entry in
  entry_label::body_vasms
;;


let get_pre_coloring (temp_func : temp_func) =
  _get_sp_temps_coloring temp_func.sp_temps
;;


(* NOTE 
 * - ignore push/pop since that changes rsp dynamically.
 * - look for negative offset only since stack grows downward,
 *   BUT RETURN POSITIVE offset for convenience. *)
let _get_max_rbp_offset_instrs (instrs : 'a instr list) : int =

  let _get_offset (offset : int) : int =
    Int.max 0 (-offset)
  in

  let _get_rbp_offset_arg (arg : 'a arg) : int =
    match arg with
    | Mem_arg (Rbp, offset) -> _get_offset offset
                        (* ignore positive offset since stack grows downward *)
    | Lbl_arg _             -> 0
    | Imm_arg _             -> 0
    | Reg_arg _             -> 0
    | Mem_arg (_, _)        -> 0
  in
  
  let _get_rbp_offset_instr (instr : 'a instr) : int =
    match instr with
    | Label _ -> 0
    | Load (arg, _) -> _get_rbp_offset_arg arg
    | Store (_, Rbp, offset) -> _get_offset offset
    | Store (_, _, _) -> 0
    | Push _ -> 0
    | Pop _ -> 0
    | IDiv _ -> 0
    | Binop (_, _, arg) -> _get_rbp_offset_arg arg
    | Cmp (arg, _) -> _get_rbp_offset_arg arg
    | Jmp _ -> 0
    | JmpC _ -> 0
    | Call _ -> 0
    | Ret ->  0
  in

  let offsets = List.map _get_rbp_offset_instr instrs in
  List.fold_left Int.max 0 offsets
;;

(* Yeah yeah internal modules; but bootstrapping takes priority *)
type spill_context =
  { next_slot      : int (* we could cache this in func, but KISS *)
  ; slot_map       : (Temp.t, int) Map.t    (* keys ∈ [temps_to_spill] *)
  ; rev_instrs     : Temp.t instr list
  ; temps_to_spill : Temp.t Set.t
  ; sp_temps       : sp_temps
  ; temp_manager   : Temp.manager
  }

let _spill_ctx_gen_temp (ctx : spill_context) : (spill_context * Temp.t) =
  let temp_manager, temp = Temp.gen ctx.temp_manager in
  let ctx = { ctx with temp_manager } in
  (ctx, temp)
;;

let _spill_ctx_get_or_alloc_slot (ctx : spill_context) (temp : Temp.t)
    : (spill_context * int) =
  match Map.get temp ctx.slot_map with
  | Some slot -> (ctx, slot)
  | None ->
    let slot = ctx.next_slot in
    let ctx = { ctx with next_slot = ctx.next_slot + 1;
                         slot_map  = Map.add temp slot ctx.slot_map }
    in (ctx, slot)
;;

let _spill_ctx_add_instr (ctx : spill_context) (instr : Temp.t instr)
  : spill_context =
  { ctx with rev_instrs = instr::ctx.rev_instrs }
;;

let _spill_slot_to_rbp_offset (slot : int) : int =
  -slot * Runtime.word_size
;;

let _spill_to_slot (ctx : spill_context) (src_temp : Temp.t) (slot : int)
  : spill_context =
  let rbp_offset = _spill_slot_to_rbp_offset slot in
  _spill_ctx_add_instr ctx (Store (Greg src_temp, Rbp, rbp_offset))
;;

let _restore_from_slot (ctx : spill_context) (slot : int) (dst_temp : Temp.t)
  : spill_context =
  let rbp_offset = _spill_slot_to_rbp_offset slot in
  let mem_arg = Mem_arg (Rbp, rbp_offset) in
  _spill_ctx_add_instr ctx (Load (mem_arg, Greg dst_temp))
;;


let _restore_all_spilled (ctx : spill_context) (temps : Temp.t Set.t)
  : (spill_context * (Temp.t, Temp.t) Map.t) =

  let _get_slot_or_err ctx temp : int =
    match Map.get temp ctx.slot_map with
    | Some slot -> slot
    | None -> failwith "[X86._restore_all_spilled] spill must precede restore"
  in

  let _restore_temp ctx temp (old_to_new_temps : (Temp.t, Temp.t) Map.t)
    : (spill_context * (Temp.t, Temp.t) Map.t) =
    let slot = _get_slot_or_err ctx temp in
    let ctx, dst_temp = _spill_ctx_gen_temp ctx in
    let ctx = _restore_from_slot ctx slot dst_temp in
    let old_to_new_temps = Map.add temp dst_temp old_to_new_temps in
    (ctx, old_to_new_temps)
  in

  Set.fold
    (fun (ctx, old_to_new_temps) temp ->
       if Set.mem temp ctx.temps_to_spill
       then _restore_temp ctx temp old_to_new_temps
       else (ctx, old_to_new_temps))
    (ctx, Temp.empty_map ())
    temps
;;

let _get_or_gen_slots_for_temps_to_spill
    (ctx : spill_context) (temps : Temp.t Set.t)
  : (spill_context * ((Temp.t * int) list))  =
  Set.fold
    (fun (ctx, temp_slot_pairs) temp ->
       if Set.mem temp ctx.temps_to_spill
       then 
         let ctx, slot = _spill_ctx_get_or_alloc_slot ctx temp in
         (ctx, (temp, slot)::temp_slot_pairs)
       else (ctx, temp_slot_pairs))
    (ctx, []) temps
;;

(* Calculate available stack slot. *)
let _spill_ctx_init temp_func (temps_to_spill : Temp.t Set.t) : spill_context =
  let max_rbp_offset = _get_max_rbp_offset_instrs temp_func.instrs in
  let max_slot = Int.ceil_div max_rbp_offset Runtime.word_size in
  { next_slot    = max_slot + 1
  ; slot_map     = Temp.empty_map ()
  ; rev_instrs   = []
  ; temps_to_spill
  ; sp_temps     = temp_func.sp_temps
  ; temp_manager = temp_func.temp_manager
  }
;;

(* For any temp to spill
 * - always load from stack before a read
 * - always store to stack after a write
 * These break up the live-interval of spilled temps into smaller chunks. *)
let _spill_instr (ctx : spill_context) (instr : Temp.t instr) : spill_context =
  (* ASSUME instruction goes like read/compute/write *)
  let reads, writes = _get_reads_and_writes_temp_instr ctx.sp_temps instr in
  let ctx, changed_temp_map = _restore_all_spilled ctx reads in
  let old_to_new_temp temp =
    match Map.get temp changed_temp_map with
    | None          -> temp
    | Some new_temp -> new_temp
  in
  let instr = _map_grs_in_instr old_to_new_temp instr in
  (* Might have restored some read temps into new temps.
   * XXX Decision: break up 1 life-interval only?
   * - always spill after write or spill only after first write, and map all
   *   other uses of spilled temp into a new temp (restore into new temp too) *)
  let ctx = _spill_ctx_add_instr ctx instr in
  (* X86's read and write regs may overlap, i.e., we can't map only read temps
   * to new temps (e.g., ADD T1 T2 -> ADD T3 T4). As a result, we might need to
   * spill new temps, but we keep track of the old ones because they are used
   * later in the program, (or we need to udpate spill set and keep another
   * map in context, too much work) *)
  let ctx, temp_slot_pairs = _get_or_gen_slots_for_temps_to_spill ctx writes in
  let temp_slot_pairs =
    List.map (fun (temp, slot) -> (old_to_new_temp temp, slot)) temp_slot_pairs
  in
  List.fold_left (* order doesn't matter *)
    (fun ctx (temp, slot) -> _spill_to_slot ctx temp slot)
  ctx temp_slot_pairs
;;

let spill_temps temp_func temps_to_spill =
  let ctx = _spill_ctx_init temp_func temps_to_spill in
  let ctx =
    List.fold_left _spill_instr ctx temp_func.instrs in
  { temp_func with instrs       = List.rev ctx.rev_instrs 
                 ; temp_manager = ctx.temp_manager
  }
;;


let _get_callee_saved_prs (pr_assignment : (Temp.t, physical_reg) Map.t)
  : physical_reg Set.t =
  Map.fold
    (fun pr callee_saved ->
       if Set.mem pr _callee_saved_physical_regs
       then Set.add pr callee_saved
       else callee_saved)
    pr_assignment
    (Set.empty _compare_physical_reg)
;;

let _instr_temp_to_pr
    (pr_assignment : (Temp.t, physical_reg) Map.t) (instr : Temp.t instr)
  : physical_reg instr =
  let temp_to_pr (temp : Temp.t) : physical_reg =
    match Map.get temp pr_assignment with
    | None -> failwith "[X86._temp_to_pr] no physical register for temp"
    | Some pr -> pr
  in
  _map_grs_in_instr temp_to_pr instr
;;

(*      ......
 * caller stack frame
 * ------------------ <- rsp % 16 = 0
 * | return address |
 * ------------------ <- RBP + 1 * word_size = old_rsp
 * |    saved rbp   | 
 * ------------------ <- RBP + 0 * word_size, rsp % 16 = 0
 *  func stack frame
 *      ......        
 *
 * NOTE
 * RSP must be aligned to 16 bytes on a 'call' instruction.
 * We maintain [rsp % 16] as an invariant after prelude, and it must be enforced
 * again before calls, e.g., after pushing args onto stack, if any. *)
let temp_func_to_func temp_func pr_assignment =
  (* calculate offset *)
  let callee_saved = Set.to_list (_get_callee_saved_prs pr_assignment) in
  let max_rbp_offset = _get_max_rbp_offset_instrs temp_func.instrs in
  let callee_saved_size = Runtime.word_size * List.length callee_saved in
  let frame_size = max_rbp_offset + callee_saved_size in
  let aligned_rbp_offset = 
    max_rbp_offset + (Int.offset_to_align frame_size Runtime.stack_alignment)
  in
  (* generate prologue and epilogue *)
  let spill_callee_saved = List.map (fun pr -> Push (Greg pr)) callee_saved in
  let restore_callee_saved =
    List.map (fun pr -> Pop (Greg pr)) (List.rev callee_saved)
  in
  let prologue =
      [ 
        Push Rbp;
        Load (Reg_arg Rsp, Rbp);
        (* TODO should subtract callee_saved_size here since they are pushed *)
        Binop (Sub, Rsp, Imm_arg aligned_rbp_offset);
      ] @ spill_callee_saved
  in
  let epilogue_label = Label.to_epilogue temp_func.entry in
  let epilogue =
    ((Label epilogue_label)::restore_callee_saved) @
    [
      Load (Reg_arg Rbp, Rsp);
      Pop Rbp;
      Ret;
    ]
  in
  (* stitch everything together *)
  let pr_instrs = List.map (_instr_temp_to_pr pr_assignment) temp_func.instrs in
  let final_instrs = prologue @ pr_instrs @ epilogue in
  { entry  = temp_func.entry;
    instrs = final_instrs }
;;


let physical_reg_to_str (r : physical_reg) : string =
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
  "[" ^ reg_str ^ op_str ^ offset_str  ^ "]"
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

let _call_target_to_str (target : 'a call_target) (gr_to_str : 'a -> string)
  : string =
  match target with
  | Reg gr -> gr_to_str gr
  | Lbl label -> Label.to_string label
;;

let _instr_to_str (instr : 'a instr) (gr_to_str : 'a -> string) : string =
  let add_tab s = "\t" ^ s in
  match instr with
  | Label label -> (Label.to_string label) ^ ":"

  | Load (arg, dst_reg) ->
    let arg_str = _arg_to_str arg gr_to_str in
    let dst_str = _reg_to_str dst_reg gr_to_str in
    let instr_str = "mov " ^ dst_str ^ ", " ^ arg_str in
    add_tab instr_str

  | Store (src_reg, dst_addr_reg, offset) ->
    let src_str = _reg_to_str src_reg gr_to_str in
    let dst_str = _reg_offset_to_str dst_addr_reg offset gr_to_str in
    let instr_str = "mov " ^ dst_str ^ ", " ^ src_str in
    add_tab instr_str

  | Push reg ->
    let reg_str = _reg_to_str reg gr_to_str in
    let instr_str = "push" ^ " " ^ reg_str in
    add_tab instr_str

  | Pop reg ->
    let reg_str = _reg_to_str reg gr_to_str in
    let instr_str = "pop" ^ " " ^ reg_str in
    add_tab instr_str

  | IDiv reg ->
    let reg_str = _reg_to_str reg gr_to_str in
    let instr_str = "idiv" ^ " " ^ reg_str in
    add_tab instr_str

  | Binop (binop, reg, arg) ->
    let arg_str = _arg_to_str arg gr_to_str in
    let reg_str = _reg_to_str reg gr_to_str in
    let binop_str = _binop_to_str binop in
    let instr_str = binop_str ^ " " ^ reg_str ^ ", " ^ arg_str in
    add_tab instr_str

  | Cmp (arg, reg) ->
    let arg_str = _arg_to_str arg gr_to_str in
    let reg_str = _reg_to_str reg gr_to_str in
    let instr_str = "cmp " ^ arg_str ^ ", " ^ reg_str in
    add_tab instr_str

  | Jmp label ->
    let instr_str = "jmp " ^ (Label.to_string label)in
    add_tab instr_str

  | JmpC (cond, label) ->
    let suffix = _cond_to_suffix cond in
    let label_str = Label.to_string label in
    let instr_str = "j" ^ suffix ^ " " ^ label_str in
    add_tab instr_str

  | Call (target, _) -> (* args are used for liveness analysis or debugging *)
    let target_str = _call_target_to_str target gr_to_str in
    let instr_str = "call" ^ " " ^ target_str in
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
  _instrs_to_str all_instrs physical_reg_to_str
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

let _add_native_labels_in_call_target 
    (labels : Label.t Set.t) (target : 'a call_target)
  : Label.t Set.t =
  match target with
  | Reg _ -> labels
  | Lbl label -> _add_label_if_native labels label
;;

let _add_external_native_labels_in_instr
    (labels : Label.t Set.t) (instr : 'a instr)
  : Label.t Set.t =
  match instr with
  | Label _           -> labels
  | Load (arg, _)     -> _add_native_labels_in_arg labels arg
  | Binop (_, _, arg) -> _add_native_labels_in_arg labels arg
  | Cmp (arg, _)      -> _add_native_labels_in_arg labels arg
  | Call (target, _)  -> _add_native_labels_in_call_target labels target

  | Store _ | Push _ | Pop _ | Jmp _ | JmpC _ | Ret | IDiv _ -> 
    labels
;;

let _add_external_native_labels_in_func
    (labels : Label.t Set.t) (func : func) : Label.t Set.t =
  List.fold_left _add_external_native_labels_in_instr labels func.instrs
;;

let _find_external_native_labels_in_prog (prog : func prog) : Label.t Set.t =
  let labels =
    List.fold_left
      _add_external_native_labels_in_func
      Label.empty_set prog.funcs
  in
  _add_external_native_labels_in_func labels prog.main
;;

let _get_prog_metadata (prog : func prog) : string =
  let external_native_label_decls = 
    List.map
      (fun label -> "extern " ^ (Label.to_string label))
      (Set.to_list (_find_external_native_labels_in_prog prog))
  in
  let global_native_label_decls =
    ["global " ^ (Label.to_string prog.main.entry)]
  in
  let metadata_lines =
      ("section .text"::external_native_label_decls) @
      global_native_label_decls
  in
  String.join_with metadata_lines "\n"
;;

(* "ur" stands for usable_register *)
let func_prog_to_str prog =
  let metadata_str = _get_prog_metadata prog in
  let all_funcs = prog.funcs @ [prog.main] in
  let func_strs = List.map _func_to_str all_funcs in
  let funcs_str = String.join_with func_strs "\n\n" in
  String.join_with [metadata_str; funcs_str] "\n\n"
;;

let temp_func_to_str temp_func =
  let all_instrs = (Label temp_func.entry)::temp_func.instrs in
  _instrs_to_str all_instrs Temp.to_string
;;
