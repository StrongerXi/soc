
(* This module contains information that needs to be synched-up with the
 * external runtime portion for compiled code. *)


(** Word size in bytes for runtime and backend *)
val word_size : int

(** rsp must be aligned to this number before call instruction *)
val stack_alignment : int

(** Entry of external function for dynamic memory allocation. *)
val mem_alloc_label : Label.t

(** Entry of compiled program. *)
val entry_label : Label.t
