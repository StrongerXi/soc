open Pervasives

(* NOTE Algorithm Essential Ideas: (essentially a greedy algorithm)
 * - First scan each call instr, pre-color all temps that live across a call to
 *   callee-saved regs, else we must spill them.
 * - Color each temp in live-in (e.g., args). (see below on how coloring works)
 * - Then scan each instruction, color each temp
 *  - allocate for read first, then write, since regs for dead read temps can
 *    be re-used, e.g., [ADD R1, R1]
 *  - spill when there isn't any color available.
 *  - accumulate active temps
 * - ignore spilled temps and continue scanning to spill as much as needed.
 * - coloring is globally fixed once upon the first assignment
 * - Termination: Worst case we spill everything, at which point we just need a
 *   minimum # of regs for each operation. If we don't even have enough regs for
 *   that, we error, because there is no possible coloring.
 *)

(* Accumulators while coloring temps in instructions *)
type 'a context =
  { avalb_colors : 'a Set.t     (* all available colors - colors of
                                   [temps_in_use].
                                   used only during coloring *)
  ; temps_in_use : Temp.t Set.t (* live temps occupying distinct regs.
                                   used only during coloring *) 
    (* The followings are needed at the end *)
  ; coloring       : (Temp.t, 'a) Map.t
  ; temps_to_spill : Temp.t Set.t      
  }

let _ctx_init
    (avalb_colors : 'a Set.t)
    (coloring : (Temp.t, 'a) Map.t)
    (temps_to_spill : Temp.t Set.t)
  : 'a context =
  { avalb_colors; coloring; temps_to_spill;
    temps_in_use = Set.empty Temp.compare; }
;;

let _ctx_find_temp_using_color (ctx : 'a context) (color : 'a) : Temp.t =
  let same_color c1 c2 = (Set.get_compare_func ctx.avalb_colors c1 c2) = 0 in
  let has_color (temp : Temp.t) = 
    match Map.get temp ctx.coloring with
    | None ->
      failwith "[Reg_alloc._ctx_find_temp_using_color] temp in use isn't colored"
    | Some temp_color -> same_color temp_color color
  in
  match Set.find has_color ctx.temps_in_use with
  | None ->
    failwith "[Reg_alloc._ctx_find_temp_using_color] no active temp uses color"
  | Some temp -> temp
;;

let _ctx_use_coloring (ctx : 'a context) (temp : Temp.t) (color : 'a)
  : 'a context =
  let avalb_colors = Set.remove color ctx.avalb_colors in
  let temps_in_use = Set.add temp ctx.temps_in_use in
  { ctx with avalb_colors; temps_in_use }
;;

let _ctx_add_active_coloring (ctx : 'a context) (temp : Temp.t) (color : 'a)
  : 'a context =
  let coloring = Map.add temp color ctx.coloring in
  let ctx = { ctx with coloring } in
  _ctx_use_coloring ctx temp color
;;

(* Spill [temp] in [ctx] and return its color; doesn't need to be in use. *)
let _ctx_spill_temp (ctx : 'a context) (temp : Temp.t)
  : ('a context * 'a) =
  match Map.get temp ctx.coloring with
  | None ->
    failwith "[Reg_alloc._ctx_spill_temp] temp to spill should be colored"
  | Some color ->
    let avalb_colors = Set.add color ctx.avalb_colors in
    let temps_in_use = Set.remove temp ctx.temps_in_use in
    let temps_to_spill = Set.add temp ctx.temps_to_spill in
    let ctx = { ctx with avalb_colors; temps_in_use; temps_to_spill } in
    (ctx, color)
;;

(* [killed] won't be used anymore *)
let _ctx_remove_dead_temps (ctx : 'a context) (killed : Temp.t Set.t)
  : 'a context =
  let temps_in_use = Set.diff ctx.temps_in_use killed in
  let avalb_colors =
    List.fold_left
      (fun avalb_colors killed_temp ->
         match Map.get killed_temp ctx.coloring with
         | None ->
           if Set.mem killed_temp ctx.temps_to_spill then avalb_colors
           else
             failwith 
               (String.join_with
                  ["[Reg_alloc._ctx_remove_killed_temps] Unspilled killed Temp "
                  ; (Temp.to_string killed_temp); " should've been colored";] "")
         | Some color ->
           Set.add color avalb_colors)
      ctx.avalb_colors (Set.to_list killed)
  in
  { ctx with temps_in_use; avalb_colors }
;;

(* ENSURE: Won't spill temps in [init_temps_to_color] *)
let _ctx_distinct_color_temps
    (ctx : 'a context) (init_temps_to_color : Temp.t Set.t)
  : 'a context =
  (* NOTE:
   * Q: Why separate [can_spill] and [ctx.temps_in_use]? 
   * A: We don't want to spill temps within the same instruction. If there's no
   * other temp we can spill, that means we simply don't have enough regs. *)
  let spill_and_color (ctx : 'a context)
      (temp_to_color : Temp.t) (can_spill : Temp.t Set.t)
    : ('a context * Temp.t Set.t) =
    match Set.get_one can_spill with
    | None -> failwith "[Reg_alloc._ctx_color_temps] Not enough register"
    | Some to_spill ->
      let ctx, color = _ctx_spill_temp ctx to_spill in
      let can_spill = Set.remove to_spill can_spill in
      let ctx = _ctx_add_active_coloring ctx temp_to_color color in
      (ctx, can_spill)
  in
  let color_uncolored_temp (ctx : 'a context)
      (temp_to_color : Temp.t) (can_spill : Temp.t Set.t)
    : ('a context * Temp.t Set.t) =
    (* pick any color, if available *)
    match Set.get_one ctx.avalb_colors with
    | None -> spill_and_color ctx temp_to_color can_spill
    | Some color ->
      let ctx = _ctx_add_active_coloring ctx temp_to_color color in
      (ctx, can_spill)
  in
  let color_one_temp (ctx : 'a context)
      (temp_to_color : Temp.t) (can_spill : Temp.t Set.t)
    : ('a context * Temp.t Set.t) =
    match Map.get temp_to_color ctx.coloring with
    | None       -> color_uncolored_temp ctx temp_to_color can_spill
    | Some color ->
      if Set.mem color ctx.avalb_colors ||
         Set.mem temp_to_color ctx.temps_in_use
      then (_ctx_use_coloring ctx temp_to_color color, can_spill)
      else (* assigned color is currently occupied by others *)
        let temp_occupying_color = _ctx_find_temp_using_color ctx color in
        let ctx, _ = _ctx_spill_temp ctx temp_occupying_color in
        (_ctx_use_coloring ctx temp_to_color color, can_spill)
  in
  let can_spill = Set.diff ctx.temps_in_use init_temps_to_color in
  Set.fold (* arbitrary order of coloring *)
    (fun (ctx, can_spill) temp ->
       if Set.mem temp ctx.temps_to_spill
       then (ctx, can_spill) (* don't waste time on already spilled temp *)
       else color_one_temp ctx temp can_spill)
    (ctx, can_spill) init_temps_to_color
  |> (fun (ctx, _) -> ctx)
;;

(* Assign distinct color to [temps] (if not colored), and free them up if not
 * alive afterwards, i.e., absent in [live_out] *)
let _ctx_distinct_color_temps_and_free_regs
  (ctx : 'a context) (temps : Temp.t Set.t) (live_out : Temp.t Set.t)
  : 'a context =
  let ctx = _ctx_distinct_color_temps ctx temps in
  let dead = Set.filter (fun temp -> not (Set.mem temp live_out)) temps in
  _ctx_remove_dead_temps ctx dead
;;

(* ASSUME temps in live-in, i.e. live-out of previous instr, were handled *)
let _brute_alloc_vasm
  (ctx : 'a context) (vasm : Vasm.t) (annot : Liveness_analysis.annot)
  : 'a context =
  let reads, writes = Vasm.get_reads vasm, Vasm.get_writes vasm in
  (* NOTE allocating reads/writes separately allows
   * T1 := T2 + T3 to use 2 regs only, but it requires the ISA to support
   * using same regs in src and dst *)
  let ctx = _ctx_distinct_color_temps_and_free_regs ctx reads annot.live_out in
  _ctx_distinct_color_temps_and_free_regs ctx writes annot.live_out
;;

let _brute_alloc_impl
    (ctx : 'a context)
    (annot_instrs   : (Vasm.t * Liveness_analysis.annot) list)
  : 'a context =
  match annot_instrs with
  | [] -> ctx
  | (_, annot)::_ ->
    (* Handle temps alive before 1st instr, think function args.
     * [arg1, ..., argn] := ...
     * All written, all alive (in the live-out of this imaginary init instr) *)
    let live_in = annot.live_in in
    let ctx = _ctx_distinct_color_temps_and_free_regs ctx live_in live_in in
    List.fold_left
      (fun ctx (instr, annot) -> _brute_alloc_vasm ctx instr annot)
      ctx annot_instrs
;;

let _brute_color_temps_live_across_call
    (init_coloring : (Temp.t, 'a) Map.t) (init_temps_to_spill : Temp.t Set.t)
    (callee_saved : 'a Set.t) (vasm : Vasm.t) (annot : Liveness_analysis.annot) 
  : ((Temp.t, 'a) Map.t * Temp.t Set.t) =

  let extract_colored_temps temp_set
    : ((Temp.t * 'a) list * Temp.t list) =
    Set.fold
      (fun (temp_color_pairs, uncolored_temps) temp ->
         match Map.get temp init_coloring with
         | Some color -> ((temp, color)::temp_color_pairs, uncolored_temps)
         | None       -> (temp_color_pairs, temp::uncolored_temps))
      ([], []) temp_set
  in

  let add_temps_colored_to_caller_saved temp_set (coloring : (Temp.t * 'a) list)
    : Temp.t Set.t =
    List.fold_left 
      (fun temps_to_spill (temp, color) ->
         if Set.mem color callee_saved then temps_to_spill
         else Set.add temp temps_to_spill)
      temp_set coloring
  in

  let color_to_callee_saved_or_spill
      (coloring : (Temp.t, 'a) Map.t)
      (temps_to_spill : Temp.t Set.t)
      (temps_to_color : Temp.t list)
    : (Temp.t, 'a) Map.t * Temp.t Set.t =
    List.fold_left (* color the rest to callee-saved, as much as possible *)
      (fun (avalb_colors, pre_colored, temps_to_spill) temp ->
         match Set.get_one avalb_colors with
         | Some color -> 
           if Set.mem temp temps_to_spill
           then (avalb_colors, pre_colored, temps_to_spill)
           else
             let avalb_colors = Set.remove color avalb_colors in
             let pre_colored = Map.add temp color pre_colored in
             (avalb_colors, pre_colored, temps_to_spill)
         (* don't waste color for already spilled temp *)
         | _ -> (avalb_colors, pre_colored, Set.add temp temps_to_spill))
      (callee_saved, coloring, temps_to_spill)
      temps_to_color
    |> (fun (_, pre_colored, temps_to_spill) -> (pre_colored, temps_to_spill))
  in

  if not (Vasm.is_call vasm) then (init_coloring, init_temps_to_spill)
  else
    (* Any temp that lives across a call must be mapped to callee-saved or
     * spilled. *)
    let temps_live_across_call = Set.inter annot.live_out annot.live_in in
    let temp_color_pairs, temps_to_color =
      extract_colored_temps temps_live_across_call
    in
    let temps_to_spill = (* must spill temps pre-colored to caller-saved *)
      add_temps_colored_to_caller_saved init_temps_to_spill temp_color_pairs
    in
    color_to_callee_saved_or_spill init_coloring temps_to_spill temps_to_color
;;

(* pre-color temps that live through Call instructions to callee-saved regs *)
let _brute_pre_color_call_temps
    (annot_instrs : (Vasm.t * Liveness_analysis.annot) list)
    (callee_saved : 'a Set.t)
    (pre_colored  : (Temp.t, 'a) Map.t)
  : ((Temp.t, 'a) Map.t * Temp.t Set.t) =
  List.fold_left
    (fun (pre_colored, temps_to_spill)  (instr, annot) ->
       _brute_color_temps_live_across_call
         pre_colored temps_to_spill callee_saved instr annot)
    (pre_colored, Set.empty Temp.compare)
    annot_instrs
;;

(* REQUIRES:
 * 1. [caller_saved] and [callee_saved] are disjoint
 * 2. anoot_instrs has accurate liveness annotation (exposed for eaiser testing)
 *)
let greedy_alloc 
    (annot_instrs : (Vasm.t * Liveness_analysis.annot) list)
    (caller_saved : 'a Set.t)
    (callee_saved : 'a Set.t)
    (pre_colored  : (Temp.t, 'a) Map.t)
  : ((Temp.t, 'a) Map.t, Temp.t Set.t) result =
  let pre_colored, temps_to_spill =
    _brute_pre_color_call_temps annot_instrs callee_saved pre_colored in
  let all_regs = Set.union caller_saved callee_saved in
  let ctx = _ctx_init all_regs pre_colored temps_to_spill in
  let ctx = _brute_alloc_impl ctx annot_instrs in
  if Set.size ctx.temps_to_spill = 0
  then Ok ctx.coloring
  else Error ctx.temps_to_spill
;;
