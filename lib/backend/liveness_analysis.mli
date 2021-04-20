open Pervasives

(* An [annot] is an annotation for a single [Vasm.t] *)
type annot =
  (* Temps whose definition before/after an instruction might be used later *)
  { live_in  : Temp.t Set.t 
  ; live_out : Temp.t Set.t 
  }


(** [analyze_vasm vasms] analyzes [vasms] in order, and annotate each [Vasm.t]
    with liveness information *)
val analyze_vasm : Vasm.t list -> (Vasm.t * annot) list
