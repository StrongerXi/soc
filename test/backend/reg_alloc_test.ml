
(*
open Pervasives

(* NOTE README 
 * We use [int] to simulate physical regs and test register allocator. So a lot
 * of the helpers here are catered to contain [int] type *)

let _empty_temp_map =
  Map.empty Temp.compare
;;

let _empty_temp_set =
  Set.empty Temp.compare
;;

let _temps_to_set (temps : Temp.t list) : Temp.t Set.t =
  List.fold_right Set.add temps (Set.empty Temp.compare)
;;

let _temp_pairs_to_map (pairs : (Temp.t * 'v) list) : (Temp.t, 'v) Map.t =
  List.fold_left (fun map (k, v) -> Map.add k v map) _empty_temp_map pairs
;;

let _err_unexpected_spill (spills : Temp.t Set.t) : 'a =
  let str = Set.to_string Temp.to_string spills in
  let msg = String.append "Unexpected spill: " str in
  OUnit2.assert_failure msg
;;

let _err_unexpected_coloring (coloring : (Temp.t, int) Map.t) : 'a =
  let str = Map.to_string Temp.to_string Int.to_string coloring in
  let msg = String.append "Unexpected coloring: " str in
  OUnit2.assert_failure msg
;;

let _int_set (items : int list) : int Set.t =
  List.fold_right Set.add items (Set.empty Int.compare)
;;

let _check_spills
    (expect_spills : Temp.t list)
    (result : ((Temp.t, int) Map.t, Temp.t Set.t) result) : unit =
  match result with
  | Error spills -> 
    let same_length = (List.length expect_spills) = (Set.size spills) in
    let is_subset =
      List.for_all
        (fun expected_temp -> Set.mem expected_temp spills)
        expect_spills
    in
    let expect_str =
      String.join_with (List.map Temp.to_string expect_spills) ", " in
    let actual_str = Set.to_string Temp.to_string spills in
    let error_msg =
      String.join_with
        ["expected: ["; expect_str; "]; actual: "; actual_str] ""
    in
    OUnit2.assert_bool error_msg (same_length && is_subset);
  | Ok coloring -> _err_unexpected_coloring coloring
;;

let _check_coloring
    (expected_coloring : (Temp.t * int) list)
    (result : ((Temp.t, int) Map.t, Temp.t Set.t) result) : unit =
  match result with
  | Error spills -> _err_unexpected_spill spills
  | Ok coloring ->
    let same_length = (List.length expected_coloring) = (Map.size coloring) in
    let is_subset =
      List.for_all
        (fun (temp, expected_color) ->
           match Map.get temp coloring with
           | None -> false
           | Some actual_color -> expected_color = actual_color)
        expected_coloring
    in
    let pair_strs =
      List.map
        (fun (temp, color) ->
           String.join_with
             ["("; Temp.to_string temp; ", "; Int.to_string color; ")"] "")
        expected_coloring
    in
    let expect_str = String.join_with pair_strs ", " in
    let actual_str = Map.to_string Temp.to_string Int.to_string coloring in
    let error_msg =
      String.join_with
        ["expected: ["; expect_str; "]; actual: "; actual_str] ""
    in
    OUnit2.assert_bool error_msg (same_length && is_subset);
;;

let _check_coloring_size
    (expect_coloring_size : int)
    (result : ((Temp.t, int) Map.t, Temp.t Set.t) result) : unit =
  match result with
  | Error spills -> _err_unexpected_spill spills
  | Ok coloring  ->
    OUnit2.assert_equal expect_coloring_size (Map.size coloring);
;;


let tests = OUnit2.(>:::) "reg_alloc_test" [

    OUnit2.(>::) "test_brute_alloc_no_precolor_no_call" (fun _ ->
        let empty_regs = _int_set [] in
        let regs = _int_set [0; 1] in
        let pre_colored = Map.empty Temp.compare in
        let manager, t0 = Temp.gen Temp.init_manager in
        let manager, t1 = Temp.gen manager in
        let manager, t2 = Temp.gen manager in

        (* allow re-using dead read regs *)
        let instrs =
          (* T0 := T1 + T2  # live-out = [] *)
          [ (Vasm.Instr 
               { reads = _temps_to_set [t1; t2];
                 writes = _temps_to_set [t0];
                 jump = None },
             { Liveness_analysis.
               live_in  = _temps_to_set [];
               live_out = _temps_to_set []; })
          ]
        in
        (* caller/callee saved shouldn't matter for these instrs *)
        _check_coloring [(t0, 1); (t1, 0); (t2, 1)]
          (Reg_alloc.brute_alloc instrs regs empty_regs pre_colored);
        _check_coloring [(t0, 1); (t1, 0); (t2, 1)]
          (Reg_alloc.brute_alloc instrs empty_regs regs pre_colored);

        (* make sure we don't spill read regs within one 1 instr *)
        let manager, t3 = Temp.gen manager in
        let _, t4 = Temp.gen manager in
        let instrs =
          (* T0 := 123
           * T1 := 456
           * T2 := T3 + T4  # live-out = [T0, T1]
           * T0  := T0 + T1  # live-out = [] *)
          [
            (Vasm.Instr
               { reads = _temps_to_set [];
                 writes = _temps_to_set [t0]; jump = None },
             { Liveness_analysis.live_out = _temps_to_set [t0; t3; t4]; });
            (Vasm.Instr
               { reads = _temps_to_set [];
                 writes = _temps_to_set [t1]; jump = None },
             { Liveness_analysis.live_out = _temps_to_set [t0; t1; t3; t4]; });
            (Vasm.Instr
               { reads = _temps_to_set [t3; t4];
                 writes = _temps_to_set [t2]; jump = None },
             { Liveness_analysis.live_out = _temps_to_set [t0; t1]; });
            (Vasm.Instr
               { reads = _temps_to_set [t0; t1];
                 writes = _temps_to_set [t0]; jump = None },
             { Liveness_analysis.live_out = _temps_to_set []; });
          ]
        in
        (* caller/callee saved shouldn't matter for these instrs *)
        _check_spills [t0; t1]
          (Reg_alloc.brute_alloc instrs regs empty_regs pre_colored);
        _check_spills [t0; t1]
          (Reg_alloc.brute_alloc instrs empty_regs regs pre_colored);
      );

    OUnit2.(>::) "test_brute_alloc_with_precolor_no_call" (fun _ ->
        let empty_regs = _int_set [] in
        let regs = _int_set [0; 1; 2] in
        let manager, t0 = Temp.gen Temp.init_manager in
        let manager, t1 = Temp.gen manager in
        let manager, t2 = Temp.gen manager in
        let _, t3 = Temp.gen manager in
        let pre_colored = _temp_pairs_to_map [(t0, 0); (t1, 1); (t2, 2)] in
        let instrs =
          (* T1 = 1         # live_out = [T1]
           * T2 = 2         # live_out = [T1, T2]
           * T3 = 3         # live_out = [T1, T2, T3]
           * T0 := T1 + T2  # live_out = [T0, T2, T3]
           * T0 + T2 + T3   # live_out = []
           * ...
           * pre-color [T0 : 0; T1 : 1; T2 : 2] prevents T1 to reuse reg,
           * forces spilling of T3 *)
          [
            (Vasm.Instr
               { reads = _temps_to_set [];
                 writes = _temps_to_set [t1]; jump = None },
             { Liveness_analysis.live_out = _temps_to_set [t1]; });
            (Vasm.Instr
               { reads = _temps_to_set [];
                 writes = _temps_to_set [t2]; jump = None },
             { Liveness_analysis.live_out = _temps_to_set [t1; t2]; });
            (Vasm.Instr
               { reads = _temps_to_set [];
                 writes = _temps_to_set [t3]; jump = None },
             { Liveness_analysis.live_out = _temps_to_set [t1; t2; t3]; });
            (Vasm.Instr
               { reads = _temps_to_set [t1; t2];
                 writes = _temps_to_set [t0]; jump = None },
             { Liveness_analysis.live_out = _temps_to_set [t0; t2; t3]; });
            (Vasm.Instr
               { reads = _temps_to_set [t0; t2; t3];
                 writes = _temps_to_set []; jump = None },
             { Liveness_analysis.live_out = _temps_to_set []; });
          ]
        in
        (* caller/callee saved shouldn't matter for these instrs *)
        _check_spills [t3]
          (Reg_alloc.brute_alloc instrs regs empty_regs pre_colored);
        _check_spills [t3]
          (Reg_alloc.brute_alloc instrs empty_regs regs pre_colored);
        _check_coloring [(t1, 0); (t2, 1); (t3, 2); (t0, 0)]
          (Reg_alloc.brute_alloc instrs regs empty_regs _empty_temp_map);
        _check_coloring [(t1, 0); (t2, 1); (t3, 2); (t0, 0)]
          (Reg_alloc.brute_alloc instrs empty_regs regs _empty_temp_map);
      );

    OUnit2.(>::) "test_brute_alloc_no_precolor_has_call" (fun _ ->
        let callee_saved = _int_set [0; 1; 2] in
        let caller_saved = _int_set [3] in
        let pre_colored = Map.empty Temp.compare in
        let manager, t0 = Temp.gen Temp.init_manager in
        let manager, t1 = Temp.gen manager in
        let manager, t2 = Temp.gen manager in
        let _, t3 = Temp.gen manager in
        let instrs =
          (* T0 := T0 + T1   # live-out = [T0, T1]
           * Call (T2, T3)   # live-out = [T0, T1] 
           * T0 := T0 + T1   # live-out = []
           * ...
           *
           * (T0, T1) must be colored to callee_saved, 
           * (T2, T3) should be able to utilize the extra callee_saved color *)
          [ 
            (Vasm.Instr
               { reads = _temps_to_set [t0; t1];
                 writes = _temps_to_set [t2]; jump = None },
             { Liveness_analysis.live_out = _temps_to_set [t0; t1]; });

            (Vasm.Call (_temps_to_set [t2; t3]),
             { Liveness_analysis.live_out = _temps_to_set [t0; t1]; });

            (Vasm.Instr
               { reads = _temps_to_set [t0; t1];
                 writes = _temps_to_set [t2]; jump = None },
             { Liveness_analysis.live_out = _temps_to_set []; });
          ]
        in
        _check_coloring [(t0, 1); (t1, 0); (t2, 3); (t3, 2)]
          (Reg_alloc.brute_alloc instrs caller_saved callee_saved pre_colored);

        let instrs =
          (* [T0 ~ T3] := _ # live-out = [T0, T1, T2, T3] 
           * Call           # live-out = [T0, T1, T2, T3] 
           * [T0 ~ T3]      # live-out = []
           *
           * one of [T0 ~ T3] needs to be spilled *)
          [ 
            (Vasm.Instr
               { reads = _temps_to_set [];
                 writes = _temps_to_set [t0; t1; t2; t3]; jump = None },
             { Liveness_analysis.live_out = _temps_to_set [t0; t1; t2; t3]; });

            (Vasm.Call (_temps_to_set []),
             { Liveness_analysis.live_out = _temps_to_set [t0; t1; t2; t3]; });

            (Vasm.Instr
               { reads = _temps_to_set [t0; t1; t2; t3];
                 writes = _temps_to_set []; jump = None },
             { Liveness_analysis.live_out = _temps_to_set []; });
          ]
        in
        (* caller/callee saved shouldn't matter for these instrs *)
        _check_spills [t0]
          (Reg_alloc.brute_alloc instrs caller_saved callee_saved pre_colored);
      );

    OUnit2.(>::) "test_brute_alloc_with_precolor_has_call" (fun _ ->
        let callee_saved = _int_set [0; 1] in
        let caller_saved = _int_set [2] in
        let manager, t0 = Temp.gen Temp.init_manager in
        let _, t1 = Temp.gen manager in
        let pre_colored = _temp_pairs_to_map [(t1, 2)] in
        let instrs =
          (*             # live-out
           * T0, T1 := _ # live-out = [T0, T1]
           * Call        # live-out = [T0, T1] 
           * T1 + T2     # live-out = []
           * ...
           *
           * - w/o pre-coloring, T0 and T1 will take up calle_saved colors,
           * - with pre-coloring T1 to callee_saved, it must be spilled. *)
          [ 
            (Vasm.Instr
               { reads = _temps_to_set [];
                 writes = _temps_to_set [t0; t1]; jump = None },
             { Liveness_analysis.live_out = _temps_to_set [t0; t1]; });

            (Vasm.Call (_temps_to_set []),
             { Liveness_analysis.live_out = _temps_to_set [t0; t1]; });

            (Vasm.Instr
               { reads = _temps_to_set [t0; t1];
                 writes = _temps_to_set []; jump = None },
             { Liveness_analysis.live_out = _temps_to_set []; });
          ]
        in
        _check_coloring [(t0, 1); (t1, 0)]
          (Reg_alloc.brute_alloc
             instrs caller_saved callee_saved _empty_temp_map);
        _check_spills [t1]
          (Reg_alloc.brute_alloc
             instrs caller_saved callee_saved pre_colored);
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
   *)
