open Pervasives

(* convenience constructors for vasm instructions *)

let _temps_to_set (temps : Temp.t list) : Temp.t Set.t =
  List.fold_right Set.add temps (Set.empty Temp.compare)
;;

let _mk_instr
    (reads : Temp.t list) (writes : Temp.t list) (jump : Vasm.jump option)
  : Vasm.t =
  Vasm.Instr
    { reads = _temps_to_set reads;
      writes = _temps_to_set writes;
      jump }
;;

let _mk_instr_no_jump (reads : Temp.t list) (writes : Temp.t list) : Vasm.t =
  _mk_instr reads writes None
;;

let _mk_instr_dir_jump
    (reads : Temp.t list) (writes : Temp.t list) (target : Label.t) : Vasm.t =
  let jump = { Vasm.target; kind = Unconditional } in
  _mk_instr reads writes (Some jump)
;;

let _mk_instr_cond_jump
    (reads : Temp.t list) (writes : Temp.t list) (target : Label.t) : Vasm.t =
  let jump = { Vasm.target; kind = Conditional } in
  _mk_instr reads writes (Some jump)
;;

let _mk_call (reads : Temp.t list) : Vasm.t =
  Vasm.Call (_temps_to_set reads)
;;

let _mk_label (label : Label.t) : Vasm.t =
  Vasm.Label label
;;


(* property checks on cfg (incomprehensive)
 * - no internal jumps or labels (besides first and last)
 * - empty node should have <= 1 in and <= 1 out neighbors
 * - last instr (check target block when applicable)
 *  - uncond jump --> <= 1 out-edge
 *  - cond jump   --> <= 2 out-edge
 *  - no jump     --> <= 1 out-edge *)
let _check_cfg_properties (cfg : Vasm.t list Graph.t) : unit =
  let _is_first_vasm_label
      (expect : Label.t option) (vasms : Vasm.t list) : bool =
    match vasms with
    | [Label actual] -> 
      begin
        match expect with
        | None -> true
        | Some expect ->
          (Label.to_string expect) = (Label.to_string actual)
      end
    | _ -> false
  in
  let _check_last_vasm_no_jump node : unit =
    match Set.to_list (Graph.get_out_neighbors cfg node) with
    | [] -> () (* last instr in the entire cfg *)
    | [next_node] -> 
      OUnit2.assert_bool
        "_check_last_vasm_no_jump"
        (_is_first_vasm_label None (Graph.get_annot cfg next_node))
    | _ -> OUnit2.assert_failure "_check_last_vasm_no_jump"
  in
  let _check_last_vasm_dir_jump node (target : Label.t) : unit =
    match Set.to_list (Graph.get_out_neighbors cfg node) with
    | [] -> () (* external jump *)
    | [next_node] ->
      let next_block_vasms = Graph.get_annot cfg next_node in
      OUnit2.assert_bool
        "_check_last_vasm_dir_jump"
        (_is_first_vasm_label (Some target) next_block_vasms)
    | _ -> OUnit2.assert_failure "_check_last_vasm_dir_jump"
  in
  let _check_last_vasm_cond_jump node (target : Label.t) : unit =
    match Set.to_list (Graph.get_out_neighbors cfg node) with
    | [] -> () (* external jump and last instr *)
    | [_] -> () (* external jump or last instr *)
    | next_node1::[next_node2] ->
      let next_block_vasms1 = Graph.get_annot cfg next_node1 in
      let next_block_vasms2 = Graph.get_annot cfg next_node2 in
      OUnit2.assert_bool
        "_check_last_vasm_cond_jump"
        ((_is_first_vasm_label (Some target) next_block_vasms1) ||
         (_is_first_vasm_label (Some target) next_block_vasms2))
    | _ -> OUnit2.assert_failure "_check_last_vasm_cond_jump"
  in
  let _check_last_instr_in_block node (instr : Vasm.instr) : unit =
    match instr.jump with
    | None -> _check_last_vasm_no_jump node
    | Some jump ->
      match jump.kind with
      | Unconditional -> _check_last_vasm_dir_jump node jump.target
      | Conditional   -> _check_last_vasm_cond_jump node jump.target
  in
  let _check_last_vasm_in_block node (prev : Vasm.t) : unit =
    match prev with
    | Call _ | Label _ -> ()
    | Instr instr -> _check_last_instr_in_block node instr
  in
  let rec _check_block_vasms
      node (prev : Vasm.t) (block_vasms : Vasm.t list)
    : unit =
    match block_vasms with
    | [] -> _check_last_vasm_in_block node prev
    | (Call reads)::rest -> _check_block_vasms node (Call reads) rest
    | (Label _)::_ ->
      OUnit2.assert_failure "label must be at the start of a cfg block"
    | (Instr instr)::rest ->
      match instr.jump with
      | Some _ ->
        OUnit2.assert_equal
          ~msg: "jump must be at the end of a cfg block" rest []
      | None -> _check_block_vasms node (Instr instr) rest
  in
  let _check_cfg_node (node : Graph.node) : unit =
    let block_vasms = Graph.get_annot cfg node in
    match block_vasms with
    | [] -> (* no in-neighbor if the block follows a direct jump *)
      OUnit2.assert_bool "expected <= 1 in-neighbor" 
        (Set.size (Graph.get_in_neighbors cfg node) <= 1);
      OUnit2.assert_bool "expected <= 1 out-neighbor" 
        (Set.size (Graph.get_out_neighbors cfg node) <= 1);
    | first_vasm::rest_vasms ->
      _check_block_vasms node first_vasm rest_vasms
  in
  List.iter _check_cfg_node (Set.to_list (Graph.get_all_nodes cfg))
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


let tests = OUnit2.(>:::) "vasm_test" [

    OUnit2.(>::) "test_get_reads_writes" (fun _ ->
        let instr_no_jump   = _mk_instr_no_jump   [t0; t2] [t0; t1]    in
        let instr_dir_jump  = _mk_instr_dir_jump  []       [t2; t3] l0 in
        let instr_cond_jump = _mk_instr_cond_jump [t2; t1] []       l1 in
        let call_no_read    = _mk_call [] in
        let call_with_read  = _mk_call [t0; t3] in
        let label           = _mk_label l0 in

        Test_aux.check_set [t0; t2] (Vasm.get_reads instr_no_jump);
        Test_aux.check_set []       (Vasm.get_reads instr_dir_jump);
        Test_aux.check_set [t2; t1] (Vasm.get_reads instr_cond_jump);
        Test_aux.check_set []       (Vasm.get_reads call_no_read);
        Test_aux.check_set [t0; t3] (Vasm.get_reads call_with_read);
        Test_aux.check_set []       (Vasm.get_reads label);

        Test_aux.check_set [t0; t1] (Vasm.get_writes instr_no_jump);
        Test_aux.check_set [t2; t3] (Vasm.get_writes instr_dir_jump);
        Test_aux.check_set []       (Vasm.get_writes instr_cond_jump);
        Test_aux.check_set []       (Vasm.get_writes call_no_read);
        Test_aux.check_set []       (Vasm.get_writes call_with_read);
        Test_aux.check_set []       (Vasm.get_writes label);
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
         *
         * NOTE reads and writes shouldn't matter for control flow *)
        let vasms =
          [
            _mk_label l0;
            _mk_instr_no_jump [t1] [t2];
            _mk_instr_cond_jump [t3] [t3] l1;
            _mk_call [t0];
            _mk_instr_dir_jump [t2; t3] [] l2;
            _mk_label l1;
            _mk_instr_dir_jump [] [t1; t3] l0;
            _mk_instr_no_jump [] [];
          ]
        in
        let cfg, ordered_nodes = Vasm.build_cfg vasms in
        let vasms_from_cfg = List.map (Graph.get_annot cfg) ordered_nodes
                              |> List.flatten in
        OUnit2.assert_equal
          ~cmp:(Test_aux.list_equal Test_aux.vasm_equal)
          vasms vasms_from_cfg;
        _check_cfg_properties cfg;
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
