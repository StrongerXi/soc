open Pervasives

val temps_to_set : Temp.t list -> Temp.t Set.t

val mk_annotated_vasms :
  Temp.t list ->                 (* live-in *)
  (Vasm.t * Temp.t list) list -> (* vasm with live-out *)
  (Vasm.t * Liveness_analysis.annot) list
