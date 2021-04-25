open Pervasives

type op =
  | Add
  | Sub
  | Mul

(* NOTE interpretation is up to instructions (as address or plain value).*)
type expr =
  | Imm of int             (* numerical constant *)
  | Tmp of Temp.t          (* temporary register *)
  | Op of op * expr * expr (* Binary operation *)
  | Call of Temp.t * expr list
      (* Call (func, args) --> func(args) *)
  | NativeCall of Label.t * expr list
      (* NativeCall (label, args) --> label(args) *)
  | Mem_alloc of int
      (* Mem_alloc (n_bytes) --> dynamic_mem_alloc(n_bytes) *)

(** A [cond] is a conditional expression used for control flow *)
type cond =
  | True
  | Less of expr * expr
  | Equal of expr * expr

(** An [instr] is a basic building block of Lir. 
    NOTE all memory addresses have byte as unit. *)
type instr =
  | Label of Label.t
  | Load of expr * Temp.t
      (* Load (e, temp) --> temp := e *)
  | LoadMem of expr * Temp.t
      (* LoadMem (e, temp) --> temp := *[e] *)
  | Store of expr * expr
      (* Store (e, dst_addr) --> *[dst_addr] := e *)
  | Store_label of Label.t * expr
      (* Store_label (label, dst_addr) --> *[dst_addr] := label *)
  | Jump of cond * Label.t
      (* Jump to label if [cond] is satisfied. *)
  | Ret of expr
      (* Return the result of [expr] to callee function *)


(** A [func] is a function at Lir level. *)
type func =
  { name         : Label.t
  ; ordered_args : Temp.t list  (* 1st temp for 1st function arg, etc. *)
  ; body         : instr list   (* NOTE doesn't start with [name] label *)
  ; temp_manager : Temp.manager
  }


(** A [prog] is the entire program at Lir level. *)
type prog =
  { funcs         : func list
  ; entry         : instr list   (* no entry label, to be generated later *)
  ; temp_manager  : Temp.manager (* for [entry] *)
  ; label_manager : Label.manager
  }


(** [from_cir_prog cir] translates a program in Cir to Lir. 
    ASSUME: All names (label or variable) are bound.
    UNDEFINED BEHAVIOR if name bindings are not unique in respective namespaces.
*)
val from_cir_prog : Cir.prog -> prog
