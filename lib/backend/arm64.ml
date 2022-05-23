open Pervasives

(* Reference:
 * https://docs.microsoft.com/en-us/cpp/build/arm64-windows-abi-conventions?view=msvc-170
 *)
type physical_reg =
  | X0
  | X1
  | X2
  | X3
  | X4
  | X5
  | X6
  | X7
  | X8
  | X9
  | X10
  | X11
  | X12
  | X13
  | X14
  | X15
  | X19
  | X20
  | X21
  | X22
  | X23
  | X24
  | X25
  | X26
  | X27
  | X28

let _compare_physical_reg r1 r2 =
  let _physical_reg_to_int (pr : physical_reg) : int =
    match pr with
    | X0 -> 0
    | X1 -> 1
    | X2 -> 2
    | X3 -> 3
    | X4 -> 4
    | X5 -> 5
    | X6 -> 6
    | X7 -> 7
    | X8 -> 8
    | X9 -> 9
    | X10 -> 10
    | X11 -> 11
    | X12 -> 12
    | X13 -> 13
    | X14 -> 14
    | X15 -> 15
    | X19 -> 16
    | X20 -> 17
    | X21 -> 18
    | X22 -> 19
    | X23 -> 20
    | X24 -> 21
    | X25 -> 22
    | X26 -> 23
    | X27 -> 24
    | X28 -> 25
  in
  Int.compare (_physical_reg_to_int r1) (_physical_reg_to_int r2)
;;

let _physical_reg_list_to_set (prs : physical_reg list) : physical_reg Set.t =
  Set.add_list prs (Set.empty _compare_physical_reg)
;;

let _ordered_argument_physical_regs =
  [X0; X1; X2; X3; X4; X5; X6; X7]
;;

let _caller_saved_physical_regs =
  _physical_reg_list_to_set
    [X0; X1; X2; X3; X4; X5; X6; X7; X8; X9; X10; X11; X12; X13; X14; X15]
;;

(* Can be used as scratch at prologue/epilogue *)
let _caller_saved_non_arg_regs =
  let ordered_arg_reg_set =
    Set.add_list
      _ordered_argument_physical_regs
      (Set.empty _compare_physical_reg)
  in
  Set.diff
    _caller_saved_physical_regs
    ordered_arg_reg_set
;;

let _callee_saved_physical_regs =
  _physical_reg_list_to_set
    [X19; X20; X21; X22; X23; X24; X25; X26; X27; X28]
;;

let _res_reg = X0
;;

let assignable_regs =
  Set.union _caller_saved_physical_regs _callee_saved_physical_regs
;;


type 'a reg =
  | Sp         (* stack pointer *)
  | X29        (* frame pointer *)
  | X30        (* return address *)
  | Greg of 'a (* general purpose registers *)

let _compare_temp_regs (r1 : Temp.t reg) (r2 : Temp.t reg) =
  let _temp_reg_to_int (reg : Temp.t reg) : int =
    match reg with
    | Sp -> 0
    | X29 -> 1
    | X30 -> 2
    | Greg _ -> 3
  in
  match (r1, r2) with
  | Greg t1, Greg t2 -> Temp.compare t1 t2
  | _ -> Int.compare (_temp_reg_to_int r1) (_temp_reg_to_int r2)
;;

type cond =
  | Eq
  | Lt

type binop =
  | Add
  | Sub
  | Mul
  | SDiv

type 'a call_target =
  | Reg of 'a
  | Lbl of Label.t

(* NOTE
 * 0. I'm taking a subset of the ARM64 ISA since we don't need the other fancy
 *    instrs for now.
 * 1. Many instructions don't support 2 memory accesses, I made that explicit.
 *)
type 'a instr =
  | Label of Label.t
  | LoadImm of 'a reg * int
      (* LoadImm (dreg, n) --> dreg := n *)
  | LoadLabelPage of 'a reg * Label.t
      (* LoadLabelPage (dreg, label) --> dreg := label@PAGE *)
  | AddLabelPageOffset of 'a reg * Label.t
      (* AddLabelPageOffset (dreg, label) --> dreg += label@PAGEOFF *)
  | Mov of 'a reg * 'a reg
      (* Mov (dreg, sreg) --> dreg := sreg *)
  | Load of 'a reg * 'a reg * 'a reg
      (* Load (dreg, breg, oreg) --> reg := *(breg + oreg) *)
  | Store of 'a reg * 'a reg * 'a reg
      (* Store (sreg, breg, oreg) --> *[breg + oreg] := sreg *)
  | Binop of binop * 'a reg * 'a reg * 'a reg
      (* Binop (op, dreg, reg1, reg2) --> dreg = op reg1 reg2 *)
  | Cmp of 'a reg * 'a reg
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
  { res              : Temp.t
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
    | None -> failwith "[ARM64._init_sp_temps] unbound physical register"
    | Some temp -> temp
  in

  let sp_prs = Set.add_list
      (_res_reg::_ordered_argument_physical_regs)
      (Set.empty _compare_physical_reg)
  in
  let sp_prs = Set.union sp_prs _caller_saved_physical_regs in
  let temp_man, pr_map = alloc_temp_for_prs init_temp_man sp_prs in
  let sp_temps = { res = get_temp pr_map _res_reg
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
    (sp_temps.res, _res_reg)::
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
  | Sp -> Sp
  | X29 -> X29
  | X30 -> X30
  | Greg gr -> Greg (f gr)
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

  | LoadLabelPage (dst_reg, label) ->
    LoadLabelPage (_map_grs_in_reg f dst_reg, label)

  | AddLabelPageOffset (dst_reg, label) ->
    AddLabelPageOffset (_map_grs_in_reg f dst_reg, label)

  | Jmp label          -> Jmp label
  | JmpC (cond, label) -> JmpC (cond, label)

  | LoadImm (dst_reg, n) -> LoadImm (_map_grs_in_reg f dst_reg, n)

  | Mov (dst_reg, src_reg) ->
    Mov (
      _map_grs_in_reg f dst_reg,
      _map_grs_in_reg f src_reg
    )

  | Load (dst_reg, base_reg, offset_reg) ->
    Load (
      _map_grs_in_reg f dst_reg,
      _map_grs_in_reg f base_reg,
      _map_grs_in_reg f offset_reg
    )

  | Store (src_reg, base_reg, offset_reg) ->
    Store (
      _map_grs_in_reg f src_reg,
      _map_grs_in_reg f base_reg,
      _map_grs_in_reg f offset_reg
    )

  | Binop (op, dst_reg, reg1, reg2) ->
    Binop (
      op,
      _map_grs_in_reg f dst_reg,
      _map_grs_in_reg f reg1,
      _map_grs_in_reg f reg2
    )

  | Cmp (reg1, reg2) ->
    Cmp (
      _map_grs_in_reg f reg1,
      _map_grs_in_reg f reg2
    )

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
  Greg ctx.sp_temps.res
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
 * This ensures arg temps are live at entry.
 * Must synch with [_emit_prepare_call_args] *)
let _emit_load_args_into_temps (ctx : context) (ordered_args : Temp.t list)
  : context =
  let load_extra_args ctx extra_arg_temps : context =
    List.fold_left
      (fun (ctx, offset_idx) arg_temp ->
         let ctx, offset_reg = _ctx_gen_temp_reg ctx in
         let ctx = _ctx_add_instrs ctx [
             LoadImm (offset_reg, offset_idx * Runtime.word_size);
             Load (Greg arg_temp, X29, offset_reg);
           ]
         in
         (ctx, offset_idx + 1))
      (ctx, 2) extra_arg_temps
    |> (fun (ctx, _) -> ctx)
  in
  let rec go ctx (arg_temps : Temp.t list) (arg_regs : Temp.t list) : context =
    match arg_temps, arg_regs with
    | [], _ -> ctx
    | _, [] -> load_extra_args ctx arg_temps
    | arg_temp::rest_arg_temps, arg_reg::rest_arg_prs ->
      let instr = Mov (Greg arg_temp, Greg arg_reg) in
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
    let instr = LoadImm (dst_reg, n) in
    _ctx_add_instr ctx instr

  | Tmp temp ->
    let instr = Mov (dst_reg, (Greg temp)) in
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
    let ctx, arg_temps, arg_bytes = _emit_prepare_call_args ctx arg_es in
    let ctx, arg_bytes_temp = _ctx_gen_temp_reg ctx in
    _ctx_add_instrs ctx [
      Call (target, arg_temps);
      LoadImm (arg_bytes_temp, arg_bytes);
      Binop (Add, Sp, Sp, arg_bytes_temp);
      Mov (dst_reg, _ctx_get_rax_reg ctx);
    ]

(*       ......
 * caller stack frame
 * ------------------
 * | 1st extra arg  |
 * ------------------ <- RBP + 2 * word_size
 * | return address |
 * ------------------ <- RBP + 1 * word_size
 * |    saved fp   |
 * ------------------ <- RBP + 0 * word_size
 * callee stack frame
 *      ......
 * NOTE stack alignment will be enforced.
 * Return
 * - Temps which holds the register arguments in order.
 *   Args spilled onto stack are ignored, since they don't need to be in any
 *   register during the call instruction -- they are saved on stack.
 * - How many bytes the aligned extra args took up on stack. *)
and _emit_prepare_call_args (init_ctx : context)
    (init_arg_es : Lir.expr list)
  : (context * Temp.t list * int) =
  (* last argument is pushed first, so 1st arg is closest to frame base *)
  let _emit_push_onto_stack ctx arg_es =
    let ctx, arg_bytes_temp = _ctx_gen_temp_reg ctx in
    let arg_bytes = Runtime.word_size * (List.length arg_es) in
    let ctx = _ctx_add_instrs ctx [
        LoadImm (arg_bytes_temp, arg_bytes);
        Binop (Sub, Sp, Sp, arg_bytes_temp);
      ]
    in
    let ctx, _ =
      List.fold_left
        (fun (ctx, idx) arg_e ->
           let ctx, arg_v_reg = _ctx_gen_temp_reg ctx in
           let ctx = _emit_lir_expr ctx arg_e arg_v_reg in
           let offset = Runtime.word_size * idx in
         let ctx, offset_reg = _ctx_gen_temp_reg ctx in
         let ctx = _ctx_add_instrs ctx [
             LoadImm (offset_reg, offset);
             Store (arg_v_reg, Sp, offset_reg);
           ]
         in (ctx, idx + 1))
        (ctx, 0) arg_es
    in
    let extra_arg_offset = (List.length arg_es) * Runtime.word_size in
    let align_offset =
      Int.offset_to_align extra_arg_offset Runtime.stack_alignment
    in (* sometimes redundant, but KISS for now *)
    let ctx, offset_temp = _ctx_gen_temp_reg ctx in
    let ctx = _ctx_add_instrs ctx [
        LoadImm (offset_temp, align_offset);
        Binop (Sub, Sp, Sp, offset_temp);
      ]
    in
    (ctx, extra_arg_offset + align_offset)
  in

  (* NOTE Hopefully coalescing or other optimization could eliminate some
   * redundant moves here. Also can consider evaluating arg-expr with most
   * nested calls first, to avoid arg-temps living across calls *)
  let move_temps_ret_dsts ctx (src_to_dst_pairs : (Temp.t reg * Temp.t) list)
    : (context * Temp.t list) =
    List.fold_left
      (fun (ctx, dsts) (src_reg, dst_temp) ->
         let instr = Mov (Greg dst_temp, src_reg) in
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
    _ctx_add_instr ctx (Binop (Add, dst_reg, dst_reg, rhs_reg))
  | Sub ->
    _ctx_add_instr ctx (Binop (Sub, dst_reg, dst_reg, rhs_reg))
  | Mul ->
    _ctx_add_instr ctx (Binop (Mul, dst_reg, dst_reg, rhs_reg))
  | Div ->
    _ctx_add_instr ctx (Binop (SDiv, dst_reg, dst_reg, rhs_reg))
;;

let _emit_comparison
    (ctx : context) (lhs_e : Lir.expr) (rhs_e : Lir.expr)
    : context =
  let ctx, lhs_reg = _ctx_gen_temp_reg ctx in
  let ctx, rhs_reg = _ctx_gen_temp_reg ctx in
  let ctx = _emit_lir_expr ctx lhs_e lhs_reg in
  let ctx = _emit_lir_expr ctx rhs_e rhs_reg in
  let instr = Cmp (lhs_reg, rhs_reg) in
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
    let ctx, offset_reg = _ctx_gen_temp_reg ctx in
    _ctx_add_instrs ctx [
      LoadImm (offset_reg, 0);
      Load (Greg dst_temp, Greg dst_temp, offset_reg)
    ]

  | Store (e, dst_addr_e) ->
    let ctx, e_reg = _ctx_gen_temp_reg ctx in
    let ctx, base_reg = _ctx_gen_temp_reg ctx in
    let ctx = _emit_lir_expr ctx e e_reg in
    let ctx = _emit_lir_expr ctx dst_addr_e base_reg in
    let ctx, offset_reg = _ctx_gen_temp_reg ctx in
    _ctx_add_instrs ctx [
      LoadImm (offset_reg, 0);
      Store (e_reg, base_reg, offset_reg);
    ]

  | Store_label (label, dst_addr_e) ->
    let ctx, base_reg = _ctx_gen_temp_reg ctx in
    let ctx = _emit_lir_expr ctx dst_addr_e base_reg in
    let ctx, label_reg = _ctx_gen_temp_reg ctx in
    let ctx, offset_reg = _ctx_gen_temp_reg ctx in
    _ctx_add_instrs ctx
      [ LoadLabelPage (label_reg, label);
        AddLabelPageOffset (label_reg, label);
        LoadImm (offset_reg, 0);
        Store (label_reg, base_reg, offset_reg); ]

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


let physical_reg_to_str (r : physical_reg) : string =
  match r with
  | X0 -> "X0"
  | X1 -> "X1"
  | X2 -> "X2"
  | X3 -> "X3"
  | X4 -> "X4"
  | X5 -> "X5"
  | X6 -> "X6"
  | X7 -> "X7"
  | X8 -> "X8"
  | X9 -> "X9"
  | X10 -> "X10"
  | X11 -> "X11"
  | X12 -> "X12"
  | X13 -> "X13"
  | X14 -> "X14"
  | X15 -> "X15"
  | X19 -> "X19"
  | X20 -> "X20"
  | X21 -> "X21"
  | X22 -> "X22"
  | X23 -> "X23"
  | X24 -> "X24"
  | X25 -> "X25"
  | X26 -> "X26"
  | X27 -> "X27"
  | X28 -> "X28"
;;

let _reg_to_str (reg : 'a reg) (gr_to_str : 'a -> string) : string =
  match reg with
  | Sp -> "SP"
  | X29 -> "X29"
  | X30 -> "X30"
  | Greg gr -> gr_to_str gr
;;


let _cond_to_suffix (cond : cond) : string =
  match cond with
  | Eq -> ".eq"
  | Lt -> ".lt"
;;

let _binop_to_str (binop : binop) : string =
  match binop with
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | SDiv -> "sdiv"
;;

let _call_target_to_str (target : 'a call_target) (gr_to_str : 'a -> string)
  : string =
  match target with
  | Reg gr -> gr_to_str gr
  | Lbl label -> Label.to_string label
;;

let _imm_to_str (offset : int) : string =
  "#" ^ (Int.to_string offset)
;;

let _reg_offset_to_str
    (base_reg : 'a reg) (offset_reg : 'a reg) (gr_to_str : 'a -> string)
  : string =
  let base_str = _reg_to_str base_reg gr_to_str in
  let offset_str = _reg_to_str offset_reg gr_to_str in
  "[" ^ base_str ^ ", " ^ offset_str ^ "]"
;;

let _instr_to_str (instr : 'a instr) (gr_to_str : 'a -> string) : string =
  let add_tab s = "\t" ^ s in
  match instr with
  | Label label -> (Label.to_string label) ^ ":"

  | LoadLabelPage (dst_reg, label) ->
    let dst_str = _reg_to_str dst_reg gr_to_str in
    let label_page_str = (Label.to_string label) ^ "@PAGE" in
    let instr_str = "adrp " ^ dst_str ^ ", " ^ label_page_str in
    add_tab instr_str

  | AddLabelPageOffset (dst_reg, label) ->
    let dst_str = _reg_to_str dst_reg gr_to_str in
    let label_page_str = (Label.to_string label) ^ "@PAGEOFF" in
    let instr_str = "add " ^ dst_str ^ ", " ^ dst_str ^ ", " ^ label_page_str in
    add_tab instr_str

  | LoadImm (dst_reg, n) -> (* use ldr pseudo instr *)
    let dst_str = _reg_to_str dst_reg gr_to_str in
    let imm_str = "=" ^ (Int.to_string n) in
    let instr_str = "ldr " ^ dst_str ^ "," ^ imm_str in
    add_tab instr_str

  | Mov (dst_reg, src_reg) ->
    let dst_str = _reg_to_str dst_reg gr_to_str in
    let src_str = _reg_to_str src_reg gr_to_str in
    let instr_str = "mov " ^ dst_str ^ ", " ^ src_str in
    add_tab instr_str

  | Load (dst_reg, base_reg, offset_reg) ->
    let dst_str = _reg_to_str dst_reg gr_to_str in
    let reg_offset_str = _reg_offset_to_str base_reg offset_reg gr_to_str in
    let instr_str = "ldr " ^ dst_str ^ ", " ^ reg_offset_str in
    add_tab instr_str

  | Store (src_reg, base_reg, offset_reg) ->
    let src_str = _reg_to_str src_reg gr_to_str in
    let reg_offset_str = _reg_offset_to_str base_reg offset_reg gr_to_str in
    let instr_str = "str " ^ src_str ^ ", " ^ reg_offset_str in
    add_tab instr_str

  | Binop (binop, dst_reg, reg1, reg2) ->
    let dst_str =  _reg_to_str dst_reg gr_to_str in
    let reg1_str =  _reg_to_str reg1 gr_to_str in
    let reg2_str =  _reg_to_str reg2 gr_to_str in
    let binop_str = _binop_to_str binop in
    let instr_str = binop_str ^ " " ^ dst_str ^ ", " ^ reg1_str ^ ", " ^ reg2_str in
    add_tab instr_str

  | Cmp (reg1, reg2) ->
    let reg1_str =  _reg_to_str reg1 gr_to_str in
    let reg2_str =  _reg_to_str reg2 gr_to_str in
    let instr_str = "cmp " ^ reg1_str ^ ", " ^ reg2_str in
    add_tab instr_str

  | Jmp label ->
    let instr_str = "b " ^ (Label.to_string label)in
    add_tab instr_str

  | JmpC (cond, label) ->
    let suffix = _cond_to_suffix cond in
    let label_str = Label.to_string label in
    let instr_str = "b" ^ suffix ^ " " ^ label_str in
    add_tab instr_str

  | Call (target, _) -> (* args are used for liveness analysis or debugging *)
    let instr_str = match target with
      | Reg gr -> "blr" ^ " " ^ (gr_to_str gr)
      | Lbl label -> "bl" ^ " " ^ (Label.to_string label)
    in
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
  let instrs_str = _instrs_to_str all_instrs physical_reg_to_str in
  let alignment_str = ".p2align 2" in
  String.join_with [alignment_str; instrs_str] "\n"
;;


let _add_label_if_native (labels : Label.t Set.t) (label : Label.t)
  : Label.t Set.t =
  if Label.is_native label
  then Set.add label labels
  else labels
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
  | Label _ | Load _ | LoadImm _ | Mov _ | Binop _ | Cmp _ | Store _ | Ret
    -> labels
  | JmpC (_, label) -> _add_label_if_native labels label
  | Jmp label -> _add_label_if_native labels label
  | LoadLabelPage (_, label) -> _add_label_if_native labels label
  | AddLabelPageOffset (_, label) -> _add_label_if_native labels label
  | Call (target, _)  -> _add_native_labels_in_call_target labels target
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
  let global_native_label_decls =
    [".globl " ^ (Label.to_string prog.main.entry)]
  in
  let metadata_lines =
      ".text"::global_native_label_decls
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

(* Ignore Sp/X29/X30 since they are managed explicitly *)
let _add_temps_in_temp_reg (acc : Temp.t Set.t) (reg : Temp.t reg)
  : Temp.t Set.t =
  match reg with
  | Sp | X29 | X30 -> acc
  | Greg temp -> Set.add temp acc
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

  | Load (dst_reg, base_reg, offset_reg) ->
    let reads = _add_temps_in_temp_reg reads base_reg in
    let reads = _add_temps_in_temp_reg reads offset_reg in
    let writes = _add_temps_in_temp_reg writes dst_reg in
    (reads, writes)

  | LoadLabelPage (dst_reg, _) ->
    let writes = _add_temps_in_temp_reg writes dst_reg in
    (reads, writes)

  | AddLabelPageOffset (dst_reg, _) ->
    let reads = _add_temps_in_temp_reg reads dst_reg in
    let writes = _add_temps_in_temp_reg writes dst_reg in
    (reads, writes)

  | LoadImm (dst_reg, _) ->
    let writes = _add_temps_in_temp_reg writes dst_reg in
    (reads, writes)

  | Mov (dst_reg, src_reg) ->
    let reads = _add_temps_in_temp_reg reads src_reg in
    let writes = _add_temps_in_temp_reg writes dst_reg in
    (reads, writes)

    (* storing to memory, so no reg is written *)
  | Store (src_reg, base_reg, offset_reg) ->
    let reads = _add_temps_in_temp_reg reads src_reg in
    let reads = _add_temps_in_temp_reg reads base_reg in
    let reads = _add_temps_in_temp_reg reads offset_reg in
    (reads, writes)

  | Binop (_, dst_reg, reg1, reg2) ->
    let reads = _add_temps_in_temp_reg reads reg1 in
    let reads = _add_temps_in_temp_reg reads reg2 in
    let writes = _add_temps_in_temp_reg writes dst_reg in
    (reads, writes)

  | Cmp (reg1, reg2) ->
    let reads = _add_temps_in_temp_reg reads reg1 in
    let reads = _add_temps_in_temp_reg reads reg2 in
    (reads, writes)

  (* Q: What about caller-saved registers?
   * A: We ignore all details about calling convention at this level (except
   *    for RAX, because our instr doesn't specify "dst_reg" of call).
   *    Basically we leave it to the register allocator to ensure that
   *    caller-saved registers are properly assigned or spilled *)
  | Call (target, reg_arg_temps) ->
    let reads = Set.add_list reg_arg_temps reads in
    let reads = _add_temps_in_call_target reads target in
    let writes = Set.add sp_temps.res writes in
    let writes = Set.union (Map.get_key_set sp_temps.caller_saved) writes in
    (reads, writes)

  | Ret ->
    let reads = Set.add sp_temps.res reads in
    (reads, writes)
;;

let _temp_instr_to_vasm (sp_temps : sp_temps) (instr : Temp.t instr) : Vasm.t =
  let reads, writes = _get_reads_and_writes_temp_instr sp_temps instr in
  let reads, writes = Set.to_list reads, Set.to_list writes in
  match instr with
  | Label label -> Vasm.mk_label label

  | Load _ | Store _ | Binop _ | Cmp _ | Call _ | LoadLabelPage _ | LoadImm _
  | AddLabelPageOffset _ | Mov _
    -> Vasm.mk_instr reads writes

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
let _get_max_fp_offset_instrs (instrs : Temp.t instr list) : int =

  let _update_ctx (ctx : (Temp.t reg, int) Map.t) (instr : 'a instr)
    : (Temp.t reg, int) Map.t =
    match instr with
    | LoadImm (dst_reg, n) -> Map.add dst_reg n ctx
    | Label _ | Load _ | Store _ | LoadLabelPage _ | AddLabelPageOffset _
    | Mov _ | Binop _ | Cmp _ | Jmp _ | JmpC _ | Call _ | Ret -> ctx
  in

  let _get_offset (ctx : (Temp.t reg, int) Map.t) (reg : 'a reg) : int =
    let offset = match Map.get reg ctx with
      | None -> failwith "[ARM64._get_max_fp_offset_instrs] Unknown reg"
      | Some(offset) -> offset
    in
    Int.max 0 (-offset)
  in

  let _get_offset_from_instr (ctx : ('a reg, int) Map.t) (instr : 'a instr)
    : int =
    match instr with
    | Load (_, X29, offset) -> _get_offset ctx offset
    | Store (_, X29, offset) -> _get_offset ctx offset
    | Label _ | Load _ | Store _ | LoadLabelPage _ | AddLabelPageOffset _
    | LoadImm _ | Mov _ | Binop _ | Cmp _ | Jmp _ | JmpC _ | Call _ | Ret -> 0
  in

  let ctx = Map.empty _compare_temp_regs in
  let ctx = List.fold_left _update_ctx ctx instrs in
  let offsets = List.map (_get_offset_from_instr ctx) instrs in
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

let _spill_ctx_add_instrs (ctx : spill_context) (instrs : Temp.t instr list)
  : spill_context =
  List.fold_left _spill_ctx_add_instr ctx instrs
;;

let _spill_slot_to_fp_offset (slot : int) : int =
  -slot * Runtime.word_size
;;

let _spill_to_slot (ctx : spill_context) (src_temp : Temp.t) (slot : int)
  : spill_context =
  let fp_offset = _spill_slot_to_fp_offset slot in
  let ctx, offset_temp = _spill_ctx_gen_temp ctx in
  let offset_reg = Greg offset_temp in
  _spill_ctx_add_instrs ctx [
    LoadImm (offset_reg, fp_offset);
    Store (Greg src_temp, X29, offset_reg);
  ]
;;

let _restore_from_slot (ctx : spill_context) (slot : int) (dst_temp : Temp.t)
  : spill_context =
  let fp_offset = _spill_slot_to_fp_offset slot in
  let ctx, offset_temp = _spill_ctx_gen_temp ctx in
  let offset_reg = Greg offset_temp in
  _spill_ctx_add_instrs ctx [
    LoadImm (offset_reg, fp_offset);
    (Load (Greg dst_temp, X29, offset_reg));
  ]
;;


let _restore_all_spilled (ctx : spill_context) (temps : Temp.t Set.t)
  : (spill_context * (Temp.t, Temp.t) Map.t) =

  let _get_slot_or_err ctx temp : int =
    match Map.get temp ctx.slot_map with
    | Some slot -> slot
    | None -> failwith "[ARM64._restore_all_spilled] spill must precede restore"
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
  let max_fp_offset = _get_max_fp_offset_instrs temp_func.instrs in
  let max_slot = Int.ceil_div max_fp_offset Runtime.word_size in
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
  (* ARM64's read and write may overlap, i.e., we can't map only read temps
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
    | None -> failwith "[ARM64._temp_to_pr] no physical register for temp"
    | Some pr -> pr
  in
  _map_grs_in_instr temp_to_pr instr
;;

(*      ......
 * caller stack frame
 * ------------------ <- rsp % 16 = 0
 * | return address |
 * ------------------ <- RBP + 1 * word_size = old_rsp
 * |    saved fp   |
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
  let max_fp_offset = _get_max_fp_offset_instrs temp_func.instrs in
  let callee_saved_size = Runtime.word_size * List.length callee_saved in
  let frame_size = max_fp_offset + callee_saved_size in
  let aligned_fp_offset =
    max_fp_offset + (Int.offset_to_align frame_size Runtime.stack_alignment)
  in
  (* generate prologue and epilogue *)
  let scratch_reg = match Set.get_one _caller_saved_non_arg_regs with
    | None -> failwith "[Arm64.temp_func_to_func] No scratch reg available"
    | Some reg -> Greg reg
  in
  let spill_callee_saved = List.flatten (
      List.mapi
        (fun idx pr -> [
             LoadImm (scratch_reg, Runtime.word_size * idx);
             Store (Greg pr, Sp, scratch_reg);])
        callee_saved
    )
  in
  let restore_callee_saved = List.flatten (
      List.mapi
        (fun idx pr -> [
             LoadImm (scratch_reg, Runtime.word_size * idx);
             Load (Greg pr, Sp, scratch_reg);])
        callee_saved
    )
  in
  let prologue =
      [
        LoadImm (scratch_reg, Runtime.word_size * 2);
        Binop (Sub, Sp, Sp, scratch_reg);
        LoadImm (scratch_reg, Runtime.word_size * 1);
        Store (X30, Sp, scratch_reg);
        LoadImm (scratch_reg, Runtime.word_size * 0);
        Store (X29, Sp, scratch_reg);

        Mov (X29, Sp);
        LoadImm (scratch_reg, aligned_fp_offset);
        Binop (Sub, Sp, Sp, scratch_reg);
      ] @ spill_callee_saved
  in
  let epilogue_label = Label.to_epilogue temp_func.entry in
  let epilogue =
    ((Label epilogue_label)::restore_callee_saved) @
    [
      Mov (Sp, X29);
      LoadImm (scratch_reg, Runtime.word_size * 0);
      Load (X29, Sp, scratch_reg);
      LoadImm (scratch_reg, Runtime.word_size * 1);
      Load (X30, Sp, scratch_reg);
      LoadImm (scratch_reg, Runtime.word_size * 2);
      Binop (Add, Sp, Sp, scratch_reg);
      Ret;
    ]
  in
  (* stitch everything together *)
  let pr_instrs = List.map (_instr_temp_to_pr pr_assignment) temp_func.instrs in
  let final_instrs = prologue @ pr_instrs @ epilogue in
  { entry  = temp_func.entry;
    instrs = final_instrs }
;;
