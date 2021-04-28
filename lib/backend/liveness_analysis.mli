open Pervasives

(* An [annot] is an annotation for a single [Vasm.t] *)
type annot =
  (* Temps whose definition before/after an instruction might be used later *)
  { live_in     : Temp.t Set.t 
  ; live_out    : Temp.t Set.t 
  (* Temps from intersection of [live_in] and [live_out] whose definition isn't
   * killed by an instruction *)
  ; live_across : Temp.t Set.t
  }


(** [analyze_vasm vasms] analyzes [vasms] in order, and annotate each [Vasm.t]
    with liveness information *)
val analyze_vasm : Vasm.t list -> (Vasm.t * annot) list
