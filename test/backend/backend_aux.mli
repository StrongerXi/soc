open Pervasives

val temps_to_set : Temp.t list -> Temp.t Set.t

(* convenience constructors for vasm instructions *)
val mk_instr : Temp.t list -> Temp.t list -> Vasm.jump option -> Vasm.t
val mk_instr_no_jump : Temp.t list -> Temp.t list -> Vasm.t
val mk_instr_dir_jump : Temp.t list -> Temp.t list -> Label.t -> Vasm.t
val mk_instr_cond_jump : Temp.t list -> Temp.t list -> Label.t -> Vasm.t
val mk_call : Temp.t list -> Temp.t list -> Vasm.t
val mk_label : Label.t -> Vasm.t
