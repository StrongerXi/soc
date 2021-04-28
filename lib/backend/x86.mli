open Pervasives

(* physical registers of X86 ISA *)
type physical_reg

(* NOTE 
 * We parameterize over general-purpose registers, i.e., virtual or physical.
 * This differentiates x86 programs before and after register allocation. *)
type 'a instr

(* A function in X86 with Temps instead of physical registers
 * NOTE without prologue/epilogue. *) 
type temp_func

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

(** [get_pre_coloring temp_func] returns a mapping for temps that 
    - must be assigned to certain physical registers 
    - cannot be spilled
    NOTE
    Since they all have short live-ranges, not spilling them shouldn't affect
    termination of register allocation. *)
val get_pre_coloring : temp_func -> (Temp.t, physical_reg) Map.t

(** Guaranteed to spill onto slots aligned by word size.
    NOTE stack is not set up, although stack slots are used.
    Final translation into [func] will set up the stack in prologue. *)
val spill_temps : temp_func -> Temp.t Set.t -> temp_func

(** [temp_func_to_func temp_func pr_assignment] returns a [func] with all
    temps in [temp_func] replaced with X86 physical register based on
    [pr_assignment].

    Also generates prologue and epilogue of a function.

    Error if any temp in [temp_func] is not in [pr_assignment]. *)
val temp_func_to_func : temp_func -> (Temp.t, physical_reg) Map.t -> func

(** [prog_to_str prog] outputs a valid X86 assembly text of [prog] *)
val prog_to_str : prog -> string

(* For debugging *)
val temp_func_to_str : temp_func -> string
val physical_reg_to_str : physical_reg -> string

(* Some pre-defined X86 physical registers *)
val callee_saved_physical_regs     : physical_reg Set.t
val caller_saved_physical_regs     : physical_reg Set.t
val ordered_argument_physical_regs : physical_reg list
val rax_physical_reg               : physical_reg
val rdx_physical_reg               : physical_reg
