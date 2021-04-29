open Pervasives

(* # Notes
 *
 * - ($...) is used for footnotes.
 *
 * ## Equation
 *
 * (1). live_out[B] := Union (live-in[S] for sucessor S of B)
 * (2). live_in[B]  := Gen[B] U (live_out[B] - Kill[B])
 * (3). Gen[B]      := temps in B that are read before written to
 * (4). Kill[B]     := temps in B that are written to
 *
 *
 * ## Intuition
 *
 * (1). If a temp is alive in _any_ path from end of B, it's alive,
 *      we must be conservative here, thus (1).
 *
 * (2). What temps are alive before entry of B? Either of the following:
 *
 *      - Temps whose definitions before entry are used in B, i.e.,
 *        before a new assignment/definition, if any, thus (2) and (3)
 *
 *      - (3) doesn't cover everything, because it only looks within B,
 *        what about after B? That's live_out[B]! But remember any
 *        definition in B kills the defined temp at live_in, thus (2) and (4).
 *
 * ## Implementation
 *
 * ### Static Gen and Kill set calculation
 *
 * Traverse [B] forward (reads happen before writes) ($)
 * - For Gen[B], we add reads that are not in Kill[B] (read before written to)
 * - Kill[B] is easy to accumulate, just add written temps.
 *
 *
 * ### Solving equation (1) and (2)
 *
 * Keep applying equation (1) and (2) until all live_in[B] and live_out[B]
 * stop changing. NOTE that
 *
 * - Order matters, we update live_out[B] first because it depends on info
 *   from other blocks, whereas live_in[B] only depends on live_out[B]
 *   (basically the propagation is backward at block level).
 *
 * - We maintain (2) as an invariant for each blokc on a worklist.
 *
 *    - During initialization, set live_out[B] to empty set ($$) and apply (2)
 *      once to establish (2) as an invariant. ($$$) Put all blocks onto the
 *      worklist.
 *
 *    - Take 1 block from worklist and apply (1) _then_ (2) to it:
 *
 *      - if live_in[B] didn't change, we know predecessors of B won't be
 *        affected by this change, nothing happens.
 *
 *      - if live_in[B] changed, the live_out sets of predecessors of B
 *        _might_ change, so we re-compute all of them. (put onto a worklist)
 *
 *    - TERMINATION: live_out[B] monotonically increases for any B if we start
 *      with empty set, since (2) is maintained as an invariant, and the set of
 *      temps is finite, (1) must converge for all blocks at some point.
 *
 *
 * ### Annotating individual instructions
 *
 * Use live_out[B] to propagate liveness info to each instr _backwards_.
 * (writes happen before reads)
 *
 * - to update live_out for next (or previous technically) instr
 *   - Remove written temps from live_out (use of these definitions end here)
 *   - Add read temps into live_out       (use of some previous definitionss)
 *
 * - If we use live_in[B] and propagate forwards, we can't immediately find
 *   out whether a newly defined temp will be used in the rest of B.
 * 
 *
 * ## Footnotes
 *
 * ($) The order of reads and writes matters, consider [T1 = T1 + 1].
 *
 * ($$)
 *      entry:          
 *        T1 = T2 + 1
 *        T2 = 42
 *      loop:           # technically T2 is dead after this point
 *        ...           # anything that doesn't contain T2
 *        jmp loop
 *
 *      If we initialize live-out with [T2], loop won't kill or gen T2
 *      Eventually we get
 *      - live-in[loop]   = [..., T2]
 *      - live-out[loop]  = [..., T2]
 *      - live-in[entry]  = [...]
 *      - live-out[entry] = [..., T2]
 *
 *      The extra T2 in live-in[loop] is problematic, especially for register
 *      allocation.
 *
 * ($$$) Technically we could do initialize live_in to empty set too, since (2)
 *       will be established after first iteration. But I think enforcing (2) at
 *       beginning is nicer to reason about, bit faster, and no more complex.
 *)

type annot =
  { live_in  : Temp.t Set.t
  ; live_out : Temp.t Set.t
  }

type block_annot =
  { live_out : Temp.t Set.t
  ; live_in  : Temp.t Set.t
  ; gen      : Temp.t Set.t
  ; kill     : Temp.t Set.t
  }

let _init_block_annot (block : Vasm.t list) : block_annot =
  let gen, kill =
    List.fold_left 
      (fun (gen, kill) (instr : Vasm.t) ->
         let reads, writes = (Vasm.get_reads instr, Vasm.get_writes instr) in
         let read_before_written = Set.diff reads kill in
         let gen = Set.union read_before_written gen in
         let kill = Set.union writes kill in
         (gen, kill))
      (Temp.empty_set, Temp.empty_set) block
  in
  let live_in, live_out = gen, Temp.empty_set in (* optimized (2) *)
  { gen; kill; live_in; live_out }
;;

(* update live_out and then live_in *)
let compute_new_block_annot cfg (node : Graph.node) : block_annot =
  let _, block_annot = Graph.get_annot cfg node in
  let gen, kill = block_annot.gen, block_annot.kill in
  let new_live_out =
    Set.fold
      (fun new_live_out succ_node -> 
         let _, succ_block_annot = Graph.get_annot cfg succ_node in
         Set.union new_live_out succ_block_annot.live_in)
      block_annot.live_out
      (Graph.get_out_neighbors cfg node)
      (* don't need to start from empty because live_out never shrinks *)
  in
  let new_live_in = Set.union gen (Set.diff new_live_out kill) in
  { block_annot with live_in = new_live_in; live_out = new_live_out }
;;

let _compute_live_in_out_until_stablize
    (init_cfg : (Vasm.t list * block_annot) Graph.t)
  : (Vasm.t list * block_annot) Graph.t =

  let rec go cfg (might_change : Graph.node Set.t)
  : (Vasm.t list * block_annot) Graph.t =
    match Set.get_one might_change with
    | None -> cfg 
    | Some node ->
      let might_change = Set.remove node might_change in
      let block, block_annot = Graph.get_annot cfg node in
      let new_block_annot = compute_new_block_annot cfg node in
      let new_node_annot = (block, new_block_annot) in
      let cfg = Graph.set_annot cfg node new_node_annot in
      (* live_out never shrinks (thus live_in too), so checking size suffices *)
      if (Set.size block_annot.live_in) = (Set.size new_block_annot.live_in)
      then go cfg might_change
      else 
        let predecessors = Graph.get_in_neighbors cfg node in
        go cfg (Set.union predecessors might_change)
  in
  go init_cfg (Graph.get_all_nodes init_cfg)
;;

let _annot_instructions_in_block
    ((block : Vasm.t list), (block_annot : block_annot))
  : (Vasm.t * annot) list =
  List.fold_right (* backward analysis, instruction order also preserved *)
    (fun (instr : Vasm.t) (annot_instrs, live_out) ->
       let reads, writes = (Vasm.get_reads instr, Vasm.get_writes instr) in
       let new_live_out = Set.diff live_out writes in
       let new_live_out = Set.union new_live_out reads in
       let annot = { live_in = new_live_out; live_out } in
       let annot_instrs = (instr, annot)::annot_instrs in
       (annot_instrs, new_live_out))
    block ([], block_annot.live_out)
  |> (fun (annot_instrs, _) -> annot_instrs)
;;

let analyze_vasm (vasms : Vasm.t list) : (Vasm.t * annot) list =
  let cfg, ordered_nodes = Vasm.build_cfg vasms in
  let cfg = Graph.map (fun block -> (block, _init_block_annot block)) cfg in
  let cfg = _compute_live_in_out_until_stablize cfg in
  let cfg = Graph.map _annot_instructions_in_block cfg in
  let ordered_blocks = List.map (Graph.get_annot cfg) ordered_nodes in
  List.flatten ordered_blocks
;;
