open Pervasives

(** [greedy_alloc instr_annot_pair available_regs pre_colored] returns 
  
    - Ok: a valid coloring for instructions in instr_annot_pair. It's guaranteed
      to be a "super-map" of [pre_colored]

    - Error: A set of temps that should be spilled, without any key from
      [pre_colored]

    It's greedy because it linear scans all the instructions and immediately
    assign available color to the temps, without backtracking or a global view.

    NOTE Undefined behavior if 
    - any liveness annotation is inaccurate. *)
val greedy_alloc :
  (Vasm.t * Liveness_analysis.annot) list ->
  'a Set.t ->
  (Temp.t, 'a) Map.t ->
  ((Temp.t, 'a) Map.t, Temp.t Set.t) result
