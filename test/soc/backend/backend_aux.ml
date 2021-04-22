open Pervasives

let temps_to_set temps =
  List.fold_right Set.add temps (Set.empty Temp.compare)
;;

let mk_annotated_vasms live_in vasm_live_out_set_pairs =
  List.fold_left
    (fun (live_in, rev_annot_instrs) (instr, live_out) ->
       let live_out = temps_to_set live_out in
       let annot = { Liveness_analysis.live_in; live_out } in
       let rev_annot_instrs = (instr, annot)::rev_annot_instrs in
       (live_out, rev_annot_instrs))
    (temps_to_set live_in, [])
    vasm_live_out_set_pairs
  |> (fun (_, rev_annot_instrs) -> List.rev rev_annot_instrs)
;;
