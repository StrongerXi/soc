open Pervasives

type jump_kind =
  | Unconditional
  | Conditional

type jump =
  { target : Label.t
  ; kind   : jump_kind
  }

(* NOTE ASSUME read happen before write *)
type instr =
  { reads  : Temp.t Set.t (* temps written to in this instr *)
  ; writes : Temp.t Set.t (* temps read from in this instr *)
  ; jump   : jump option  (* whether this instruction jumps to some label *)
  }

type t =
  | Instr of instr
  | Label of Label.t
  | Call  of Temp.t Set.t (* reads *)


(* Some query functions over [t] *)
val get_reads  : t -> Temp.t Set.t
val get_writes : t -> Temp.t Set.t


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
