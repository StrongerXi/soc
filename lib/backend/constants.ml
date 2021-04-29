
(* This module contains some constants for runtime and backend *)


(** Word size in bytes for runtime and backend *)
let word_size = 8
;;

(** rsp must be aligned to this number before call instruction *)
let stack_alignment = 16
;;


(* NOTE they must synch up with C runtime's export definition. *)

(** dynamic memory allocation *)
let mem_alloc_label = Label.get_native "mem_alloc"
;;

(** program entry *)
let entry_label = Label.get_native "entry"
;;
