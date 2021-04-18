open Pervasives

(* An [annot] is an annotation for a single [Vasm.t] *)
type annot =
  { live_out : Temp.t Set.t (* Temps whose definition at a [Vasm.t] might be
                               used later *)
  }


(** [analyze_vasm vasms] analyzes [vasms] in order, and annotate each [Vasm.t]
    with liveness information *)
val analyze_vasm : Vasm.t list -> (Vasm.t * annot) list
