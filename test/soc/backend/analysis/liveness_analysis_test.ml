open Pervasives

let _annotated_vasm_equal
  ((v1 : Vasm.t), (a1 : Liveness_analysis.annot))
  ((v2 : Vasm.t), (a2 : Liveness_analysis.annot))
  : bool =
  Vasm.equal v1 v2 &&
  Set.equal a1.live_out a2.live_out
;;


(* example temps and labels for convenience *)
let t0, t1, t2, t3 =
  let temp_manager, t0 = Temp.gen Temp.init_manager in
  let temp_manager, t1 = Temp.gen temp_manager in
  let temp_manager, t2 = Temp.gen temp_manager in
  let _, t3            = Temp.gen temp_manager in
  (t0, t1, t2, t3)
;;

let l0, l1, l2 = 
  let label_manager, l0 = Label.gen Label.init_manager "L" in
  let label_manager, l1 = Label.gen label_manager "L" in
  let _, l2             = Label.gen label_manager "L" in
  (l0, l1, l2)
;;

let tests = OUnit2.(>:::) "Liveness_analysis_test" [

    OUnit2.(>::) "test_analyze_vasm_linear" (fun _ ->
        (*                    # live-out: [t0]
         * t0     -> t0, t1   # live-out: [t1, t3]
         * t3     -> t2       # live-out: [t1, t3]
         * t1, t3 -> ...      # live-out: [t1]
         * ...    -> t2, t3   # live-out: [t1, t3]
         * t1, t3 -> ...      # live-out: []
         *
         * - t0: immediately killed
         * - t1: lived throughout
         * - t2: defined but never used
         * - t3: used before and after definition
         *)
        let expected = Backend_aux.mk_annotated_vasms [t0]
            [
              (Vasm.mk_instr [t0] [t0; t1], [t1; t3]);
              (Vasm.mk_instr [t3] [t2],     [t1; t3]);
              (Vasm.mk_instr [t1; t3] [],   [t1]);
              (Vasm.mk_instr [] [t2; t3],   [t1; t3]);
              (Vasm.mk_instr [t1; t3] [],   []);
            ]
        in
        let vasms = List.map (fun (instr, _) -> instr) expected in
        let annotated_vasms = Liveness_analysis.analyze_vasm vasms in
        OUnit2.assert_equal
          ~printer:Pretty.pp_vasm_liveness_annot
          ~cmp:(List.equal _annotated_vasm_equal)
          expected annotated_vasms
      );

    OUnit2.(>::) "test_analyze_vasm_with_jumps" (fun _ ->
        (* ### High-level CFG:
         *
         * ---
         * v /
         * B0    # L0
         * | ^      
         * |  \
         * v  |
         * B1 |  # no label start after cond jump 
         *    X
         * B2 |  # no label start after cond jump
         *    /
         *   v
         * B3    # L1
         *   
         *
         * ### Actual Instructions:
         *
         * B0: (gen = [t0], kill = [t0, t1], pred = [B0, B2], succ = [B0, B1],
         *      live-in = [t0], live-out = [t0, t1])
         *  L0                  # live-out: [t0]
         *   t0     -> t0, t1   # live-out: [t0, t1]
         *   t0, t1 -> ...      # live-out: [t0, t1]
         *   cond_jump L0       # live-out: [t0, t1]
         *
         * B1: (gen = [t0], kill = [t0], pred = [B0], succ = [B2, B3],
         *      live-in = [t0, t1], live-out = [t0, t1])
         *   t0     -> t0       # live-out: [t0, t1]
         *   cond_jump L1       # live-out: [t0, t1]
         *
         * B2: (gen = [t0, t1], kill = [t0, t2], pred = [B1], succ = [B0],
         *      live-in = [t0, t1], live-out = [t0])
         *   t0, t1 -> t2       # live-out: [t2]
         *   t2     -> t0       # live-out: [t0, t2]
         *   dir_jump t2, L0    # live-out: [t0]
         *
         * B3: (gen = [t0], kill = [t0], pred = [B1], succ = [],
         *      live-in = [t0], live-out = [])
         *  L1                  # live-out: [t0]
         *   t0     -> t3       # live-out: [t0, t3]
         *   t0, t3 -> t0       # live-out: []
         *)
        let expected = Backend_aux.mk_annotated_vasms [t0]
            [ (* B0 *)
              (Vasm.mk_label l0,            [t0]);
              (Vasm.mk_instr [t0] [t0; t1], [t0; t1]);
              (Vasm.mk_instr [t0; t1] [],   [t0; t1]);
              (Vasm.mk_cond_jump [] [] l0,  [t0; t1]);
              (* B1 *)
              (Vasm.mk_instr [t0] [t0],    [t0; t1]);
              (Vasm.mk_cond_jump [] [] l1, [t0; t1]);
              (* B2 *)
              (Vasm.mk_instr [t0; t1] [t2], [t2]);
              (Vasm.mk_instr [t2] [t0],     [t0; t2]);
              (Vasm.mk_dir_jump [t2] [] l0, [t0]);
              (* B3 *)
              (Vasm.mk_label l1,            [t0]);
              (Vasm.mk_instr [t0] [t3],     [t0; t3]);
              (Vasm.mk_instr [t0; t3] [t0], []);
            ]
        in
        let vasms = List.map (fun (instr, _) -> instr) expected in
        let annotated_vasms = Liveness_analysis.analyze_vasm vasms in
        OUnit2.assert_equal
          ~printer:Pretty.pp_vasm_liveness_annot
          ~cmp:(List.equal _annotated_vasm_equal)
          expected annotated_vasms
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
