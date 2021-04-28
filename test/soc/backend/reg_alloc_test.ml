open Pervasives

(* NOTE README 
 * We use [int] to simulate physical regs and test register allocator. So a lot
 * of the helpers here are catered to contain [int] type *)

let _empty_temp_map =
  Map.empty Temp.compare
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
      String.concat ["expected: ["; expect_str; "]; actual: "; actual_str]
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
           String.concat
             ["("; Temp.to_string temp; ", "; Int.to_string color; ")"])
        expected_coloring
    in
    let expect_str = String.join_with pair_strs ", " in
    let actual_str = Map.to_string Temp.to_string Int.to_string coloring in
    let error_msg =
      String.concat ["expected: ["; expect_str; "]; actual: "; actual_str]
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

    OUnit2.(>::) "test_greedy_alloc_no_precolor" (fun _ ->
        let regs = _int_set [0; 1] in
        let pre_colored = Map.empty Temp.compare in
        let manager, t0 = Temp.gen Temp.init_manager in
        let manager, t1 = Temp.gen manager in
        let manager, t2 = Temp.gen manager in

        (* allow re-using dead read regs
         * ...            # live-out = [T1; T2]
         * T0 := T1 + T2  # live-out = [] *)
        let instrs = Backend_aux.mk_annotated_vasms [t1; t2]
            [
              (Vasm.mk_instr [t1; t2] [t0], []);
            ]
        in
        _check_coloring [(t0, 1); (t1, 0); (t2, 1)]
          (Reg_alloc.greedy_alloc instrs regs pre_colored);

        (* spill live-in *)
        let manager, t3 = Temp.gen manager in
        let _, t4 = Temp.gen manager in
        (* ...             # live-out = [T3; T4]
         * T0 := 123       # live-out = [T0; T3; T4]
         * T1 := 456       # live-out = [T0; T1; T3; T4]
         * T2 := T3 + T4   # live-out = [T0, T1]
         * T0 := T0 + T1   # live-out = [] *)
        let instrs = Backend_aux.mk_annotated_vasms [t3; t4]
            [
              (Vasm.mk_instr []       [t0], [t0; t3; t4]);
              (Vasm.mk_instr []       [t1], [t0; t1; t3; t4]);
              (Vasm.mk_instr [t3; t4] [t2], [t0; t1]);
              (Vasm.mk_instr [t0; t1] [t0], []);
            ]
        in
        _check_spills [t0; t4]
          (Reg_alloc.greedy_alloc instrs regs pre_colored);
      );

    OUnit2.(>::) "test_greedy_alloc_with_precolor" (fun _ ->
        let regs = _int_set [0; 1; 2] in
        let manager, t0 = Temp.gen Temp.init_manager in
        let manager, t1 = Temp.gen manager in
        let manager, t2 = Temp.gen manager in
        let _, t3 = Temp.gen manager in
        let pre_colored = _temp_pairs_to_map [(t0, 0); (t1, 1); (t2, 2)] in
        (* ...            # live_out = []
         * T1 = 1         # live_out = [T1]
         * T2 = 2         # live_out = [T1, T2]
         * T3 = 3         # live_out = [T1, T2, T3]
         * T0 := T1 + T2  # live_out = [T0, T2, T3]
         * T0 + T2 + T3   # live_out = []
         * ...
         * pre-color [T0 : 0; T1 : 1; T2 : 2] prevents T1 to reuse reg,
         * forces spilling of T3 *)
        let instrs = Backend_aux.mk_annotated_vasms []
            [
              (Vasm.mk_instr []       [t1], [t1;]);
              (Vasm.mk_instr []       [t2], [t1; t2;]);
              (Vasm.mk_instr []       [t3], [t1; t2; t3;]);
              (Vasm.mk_instr [t1; t2] [t0], [t0; t2; t3;]);
              (Vasm.mk_instr [t2; t3] [t0], []);
            ]
        in
        _check_spills [t3]
          (Reg_alloc.greedy_alloc instrs regs pre_colored);
        _check_coloring [(t1, 0); (t2, 1); (t3, 2); (t0, 0)]
          (Reg_alloc.greedy_alloc instrs regs _empty_temp_map);
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
