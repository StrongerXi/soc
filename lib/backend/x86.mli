open Pervasives

(* physical registers of X86 ISA *)
type physical_reg

(* Some physical registers can't be used for register allocation *)
type 'a reg =
  | Rsp        (* not sure how allowing this in reg-alloc would affect things *)
  | Rbp        (* we need rbp for spilling *)
  | Greg of 'a (* general purpose registers *)

(* argument to x86 instructions *)
type 'a arg =
  | Lbl_arg of Label.t      (* label, e.g., use function label as data *)
  | Imm_arg of int          (* immediate, i.e., constants*)
  | Reg_arg of 'a reg       (* load from [reg] *)
  | Mem_arg of 'a reg * int (* load from address [reg + offset] *)

(* for conditional operations *)
type cond =
  | Eq
  | Lt

(* for binary operations *)
type binop =
  | Add
  | Sub
  | Mul

(* NOTE 
 * 0. We parameterize over general-purpose registers, i.e., virtual or physical.
 *    This differentiates x86 programs before and after register allocation.
 * 1. I'm not being truthful to the X86 ISA here. I really took a subset of it
 *    for simplicity, and since we don't need the other fancy instrs for now.
 * 2. Many instructions don't support 2 memory accesses, I made that explicit.
 *)
type 'a instr = 
  | Label of Label.t
  | Load of 'a arg * 'a reg
      (* Load (arg, reg) --> reg := arg *)
  | Store of 'a arg * 'a reg * int
      (* Store (arg, reg, offset) --> *[reg + offset] := arg *)
  | Push of 'a
  | Pop of 'a
  | Binop of binop * 'a * 'a arg
      (* Binop (op, reg, arg) --> reg := op gr arg *)
  | Cmp of 'a arg * 'a
  | Jmp of Label.t
  | JmpC of cond * Label.t
      (* Jump to label if [cond] is satisfied. *)
  | Call_reg of 'a
  | Call_lbl of Label.t
  | SetC of cond * 'a
      (* Sets content of ['a] to 1 if [cond] is satisfied, else 0 *)
  | Ret
      (* Return control flow to caller site *)


(* A function in X86 with Temps and some annotation to help reg-alloc.
 * NOTE without prologue/epilogue. *) 
type temp_func = 
  { entry  : Label.t
  ; instrs : Temp.t instr list (* doesn't start with [entry] label *)
  ; args   : Temp.t list
  ; rax    : Temp.t
  }

(* A program in X86 before register allocation *)
type temp_prog = 
  { temp_funcs : temp_func list
  ; temp_main  : temp_func
  }


(* A function unit in X86 after register allocation
 * NOTE with prologue/epilogue; this is the final assembly code. *)
type func =
  { entry  : Label.t
  ; instrs : physical_reg instr list (* doesn't start with [entry] label *)
  }

(* An entire program in X86 after register allocation *)
type prog = 
  { funcs : func list
  ; main : func
  }


val from_lir_prog : Lir.prog -> temp_prog

(** Includes entry label in output vasms. *)
val temp_func_to_vasms : temp_func -> Vasm.t list

(** Guaranteed to spill onto slots aligned by word size.
    NOTE stack is not set up, although stack slots are used.
    Final translation into [func] will set up the stack in prologue. *)
val spill_temps : temp_func -> Temp.t Set.t -> temp_func
