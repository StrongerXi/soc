open Pervasives

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


let tests = OUnit2.(>:::) "vasm_test" [

    OUnit2.(>::) "test_query_funcs" (fun _ ->
        let instr_no_jump    = Vasm.mk_instr [t0; t2] [t0; t1] in
        let instr_dir_jump   = Vasm.mk_dir_jump [] [t2; t3] l0 in
        let instr_cond_jump  = Vasm.mk_cond_jump [t2; t1] [] l1 in
        let call_no_read     = Vasm.mk_call [] [] in
        let call_with_read   = Vasm.mk_call [t0; t3] [] in
        let call_with_writes = Vasm.mk_call [t0] [t2; t1] in
        let ret_no_read      = Vasm.mk_ret [] [] in
        let ret_with_read    = Vasm.mk_ret [t1] [] in
        let ret_with_writes  = Vasm.mk_ret [t0] [t1; t2] in
        let label            = Vasm.mk_label l0 in

        Test_aux.check_set [t0; t2] (Vasm.get_reads instr_no_jump);
        Test_aux.check_set []       (Vasm.get_reads instr_dir_jump);
        Test_aux.check_set [t2; t1] (Vasm.get_reads instr_cond_jump);
        Test_aux.check_set []       (Vasm.get_reads call_no_read);
        Test_aux.check_set [t0; t3] (Vasm.get_reads call_with_read);
        Test_aux.check_set [t0]     (Vasm.get_reads call_with_writes);
        Test_aux.check_set []       (Vasm.get_reads ret_no_read);
        Test_aux.check_set [t1]     (Vasm.get_reads ret_with_read);
        Test_aux.check_set [t0]     (Vasm.get_reads ret_with_writes);
        Test_aux.check_set []       (Vasm.get_reads label);

        Test_aux.check_set [t0; t1] (Vasm.get_writes instr_no_jump);
        Test_aux.check_set [t2; t3] (Vasm.get_writes instr_dir_jump);
        Test_aux.check_set []       (Vasm.get_writes instr_cond_jump);
        Test_aux.check_set []       (Vasm.get_writes call_no_read);
        Test_aux.check_set []       (Vasm.get_writes call_with_read);
        Test_aux.check_set [t2; t1] (Vasm.get_writes call_with_writes);
        Test_aux.check_set []       (Vasm.get_writes ret_no_read);
        Test_aux.check_set []       (Vasm.get_writes ret_with_read);
        Test_aux.check_set [t1; t2] (Vasm.get_writes ret_with_writes);
        Test_aux.check_set []       (Vasm.get_writes label);

        OUnit2.assert_equal false (Vasm.is_call instr_no_jump);
        OUnit2.assert_equal false (Vasm.is_call instr_dir_jump);
        OUnit2.assert_equal false (Vasm.is_call instr_cond_jump);
        OUnit2.assert_equal true  (Vasm.is_call call_no_read);
        OUnit2.assert_equal true  (Vasm.is_call call_with_read);
        OUnit2.assert_equal true  (Vasm.is_call call_with_writes);
        OUnit2.assert_equal false (Vasm.is_call ret_no_read);
        OUnit2.assert_equal false (Vasm.is_call ret_with_read);
        OUnit2.assert_equal false (Vasm.is_call ret_with_writes);
        OUnit2.assert_equal false (Vasm.is_call label);

        (* reflexivity *)
        OUnit2.assert_equal true (Vasm.equal instr_no_jump instr_no_jump);
        OUnit2.assert_equal true (Vasm.equal instr_dir_jump instr_dir_jump);
        OUnit2.assert_equal true (Vasm.equal instr_cond_jump instr_cond_jump);
        OUnit2.assert_equal true (Vasm.equal call_no_read call_no_read);
        OUnit2.assert_equal true (Vasm.equal call_with_read call_with_read);
        OUnit2.assert_equal true (Vasm.equal call_with_writes call_with_writes);
        OUnit2.assert_equal true (Vasm.equal ret_no_read ret_no_read);
        OUnit2.assert_equal true (Vasm.equal ret_with_read ret_with_read);
        OUnit2.assert_equal true (Vasm.equal ret_with_writes ret_with_writes);
        OUnit2.assert_equal true (Vasm.equal label label);

        OUnit2.assert_equal true
          (Vasm.equal instr_no_jump (Vasm.mk_instr [t0; t2] [t0; t1]));
        OUnit2.assert_equal true
          (Vasm.equal instr_dir_jump instr_dir_jump);
        OUnit2.assert_equal true
          (Vasm.equal instr_cond_jump (Vasm.mk_cond_jump [t2; t1] [] l1));
        OUnit2.assert_equal true
          (Vasm.equal call_no_read (Vasm.mk_call [] []));
        OUnit2.assert_equal true
          (Vasm.equal call_with_read (Vasm.mk_call [t0; t3] []));
        OUnit2.assert_equal true
          (Vasm.equal call_with_writes (Vasm.mk_call [t0] [t2; t1]));
        OUnit2.assert_equal true
          (Vasm.equal ret_no_read (Vasm.mk_ret [] []));
        OUnit2.assert_equal true
          (Vasm.equal ret_with_read (Vasm.mk_ret [t1] []));
        OUnit2.assert_equal true
          (Vasm.equal ret_with_writes (Vasm.mk_ret [t0] [t1; t2]));
        OUnit2.assert_equal true
          (Vasm.equal label (Vasm.mk_label l0));

        (* not equal to others *)
        OUnit2.assert_equal false (Vasm.equal instr_no_jump instr_dir_jump);
        OUnit2.assert_equal false (Vasm.equal instr_dir_jump instr_cond_jump);
        OUnit2.assert_equal false (Vasm.equal instr_cond_jump call_no_read);
        OUnit2.assert_equal false (Vasm.equal call_no_read call_with_read);
        OUnit2.assert_equal false (Vasm.equal call_with_read call_with_writes);
        OUnit2.assert_equal false (Vasm.equal call_with_writes ret_no_read);
        OUnit2.assert_equal false (Vasm.equal ret_no_read ret_with_read);
        OUnit2.assert_equal false (Vasm.equal ret_with_read ret_with_writes);
        OUnit2.assert_equal false (Vasm.equal ret_with_writes label);
        OUnit2.assert_equal false (Vasm.equal label instr_no_jump);
      );

    OUnit2.(>::) "test_build_cfg" (fun _ ->
        (* L0:
         *    instr        # filler
         *    cond_jump L1 # forward jump with condition, jump after label
         *    call         # call after jump
         *    dir_jump L2  # external jump
         * L1:             # label after jump
         *    dir_jump L0  # backward jump w/o condition
         *    instr        # instr after jump
         *    ret          
         *    instr        # instr after ret, unreachable but should be collected
         *
         * NOTE reads and writes shouldn't matter for control flow *)
        let vasms =
          [
            Vasm.mk_label l0;
            Vasm.mk_instr [t1] [t2];
            Vasm.mk_cond_jump [t3] [t3] l1;
            Vasm.mk_call [t0] [];
            Vasm.mk_dir_jump [t2; t3] [] l2;
            Vasm.mk_label l1;
            Vasm.mk_dir_jump [] [t1; t3] l0;
            Vasm.mk_instr [] [];
            Vasm.mk_ret [t1] [];
            Vasm.mk_instr [t2] [t2];
          ]
        in
        let cfg, ordered_nodes = Vasm.build_cfg vasms in
        let vasms_from_cfg = List.map (Graph.get_annot cfg) ordered_nodes
                              |> List.flatten in
        OUnit2.assert_equal
          ~cmp:(Test_aux.list_equal Vasm.equal)
          vasms vasms_from_cfg;
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
