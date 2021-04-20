open Pervasives

let temps_to_set (temps : Temp.t list) : Temp.t Set.t =
  List.fold_right Set.add temps (Set.empty Temp.compare)
;;

let mk_instr
    (reads : Temp.t list) (writes : Temp.t list) (jump : Vasm.jump option)
  : Vasm.t =
  Vasm.Instr
    { reads = temps_to_set reads;
      writes = temps_to_set writes;
      jump }
;;

let mk_instr_no_jump (reads : Temp.t list) (writes : Temp.t list) : Vasm.t =
  mk_instr reads writes None
;;

let mk_instr_dir_jump
    (reads : Temp.t list) (writes : Temp.t list) (target : Label.t) : Vasm.t =
  let jump = { Vasm.target; kind = Unconditional } in
  mk_instr reads writes (Some jump)
;;

let mk_instr_cond_jump
    (reads : Temp.t list) (writes : Temp.t list) (target : Label.t) : Vasm.t =
  let jump = { Vasm.target; kind = Conditional } in
  mk_instr reads writes (Some jump)
;;

let mk_call (reads : Temp.t list) (writes : Temp.t list): Vasm.t =
  Vasm.Call (temps_to_set reads, temps_to_set writes)
;;

let mk_label (label : Label.t) : Vasm.t =
  Vasm.Label label
;;


