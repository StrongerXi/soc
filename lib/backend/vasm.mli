open Pervasives

(** A [t] is a virtual assembly instruction, which tries to abstract over
    different backend ISAs. *)
type t


(* Some constructors; generally take in lists of read/written temps *)
val mk_label     : Label.t -> t
val mk_instr     : Temp.t list -> Temp.t list -> t
val mk_dir_jump  : Temp.t list -> Temp.t list -> Label.t -> t
val mk_cond_jump : Temp.t list -> Temp.t list -> Label.t -> t
val mk_ret       : Temp.t list -> Temp.t list -> t
val mk_call      : Temp.t list -> Temp.t list -> t


(* Some query functions over [t] *)
val get_reads    : t -> Temp.t Set.t
val get_writes   : t -> Temp.t Set.t
val is_call      : t -> bool

(** structural equality *)
val equal : t -> t -> bool


(** Returns 
    - a primitive control flow graph where each node is annotated with all
      the instructions in it, in order.

    - a list of nodes based on order of input instructions; in other words,
      one can use [Graph.get_annot] to linearize the CFG to its original
      instruction list.
  
    A block starts at either
      - beginning
      - a label
      - after a conditional or direct jump to some label
   
    NOTE
    1. External jumps (e.g., for tail calls) will be considered end of block,
       because we don't do inter-procedural analysis.
    2. We might end up with some empty intermediate blocks in the output graph;
       we could eliminate them by searching and merging, but KISS for now. *)
val build_cfg  : t list -> (t list Graph.t * Graph.node list)


(** [pp t] returns a readable string representation of [t] *)
val pp : t -> string
