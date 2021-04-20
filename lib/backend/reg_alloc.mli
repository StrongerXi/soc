open Pervasives

(** [greedy_alloc instr_annot_pair caller_saved callee_saved pre_colored] returns 
  
    - Ok: a valid coloring for instructions in instr_annot_pair. It's guaranteed
      to be a "super-map" of [pre_colored]

    - Error: A set of temps that should be spilled (may spill more or less than
      what's needed, but repeated application will ensure termination, I hope) 

    It's greedy because it linear scans all the instructions and immediately
    assign available color to the temps, without backtracking or a global view.

    NOTE Undefined behavior if 
    - [caller_saved] and [callee_saved] sets are not disjoint, 
    - any liveness annotation is inaccurate. *)
val greedy_alloc :
  (Vasm.t * Liveness_analysis.annot) list ->
  'a Set.t -> 'a Set.t ->
  (Temp.t, 'a) Map.t ->
  ((Temp.t, 'a) Map.t, Temp.t Set.t) result
