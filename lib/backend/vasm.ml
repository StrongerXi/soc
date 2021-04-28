open Pervasives

type jump_kind =
  | Unconditional
  | Conditional

type jump =
  { target : Label.t
  ; kind   : jump_kind
  }

(* NOTE update equal method when updating this *)
type instr_desc =
  | Jump of jump
  | Linear
  | Ret

type instr =
  { reads  : Temp.t Set.t 
  ; writes : Temp.t Set.t 
  ; desc   : instr_desc
  }

type t =
  | Label of Label.t
  | Instr of instr



let mk_label label = 
  Label label
;;

let _mk_instr (reads : Temp.t list) (writes : Temp.t list) (desc : instr_desc)
  : t =
  let _temps_to_set (temps : Temp.t list) : Temp.t Set.t =
    List.fold_right Set.add temps (Set.empty Temp.compare)
  in
  Instr { reads  = _temps_to_set reads;
          writes = _temps_to_set writes;
          desc }
;;

let mk_instr reads writes =
  _mk_instr reads writes Linear
;;

let mk_dir_jump reads writes target =
  let jump = { target; kind = Unconditional } in
  _mk_instr reads writes (Jump jump)
;;

let mk_cond_jump reads writes target =
  let jump = { target; kind = Conditional } in
  _mk_instr reads writes (Jump jump)
;;

let mk_ret reads writes =
  _mk_instr reads writes Ret
;;


let get_reads t : Temp.t Set.t =
  match t with
  | Label _     -> Set.empty Temp.compare
  | Instr instr -> instr.reads
;;

let get_writes t : Temp.t Set.t =
  match t with
  | Label _     -> Set.empty Temp.compare
  | Instr instr -> instr.writes
;;


(* TODO move to Set module *)
let _set_equal (s1 : 'a Set.t) (s2 : 'a Set.t) : bool =
  let s1_size = Set.size s1 in
  (s1_size = Set.size s2) &&
  (Set.size (Set.union s1 s2) = s1_size)
;;

let _jump_equal (j1 : jump) (j2 : jump) : bool =
  ((Label.to_string j1.target) = (Label.to_string j2.target)) &&
  (j1 = j2)
;;

let _instr_desc_equal (d1 : instr_desc) (d2 : instr_desc) : bool =
  match d1, d2 with
  | Linear, Linear   -> true
  | Ret, Ret         -> true
  | Jump j1, Jump j2 -> _jump_equal j1 j2
  | _, _ -> false
;;

let _instr_equal (i1 : instr) (i2 : instr) : bool =
  (_set_equal i1.reads i2.reads) &&
  (_set_equal i1.writes i2.writes) &&
  (_instr_desc_equal i1.desc i2.desc)
;;

let equal t1 t2 =
  match t1, t2 with
  | Label l1, Label l2 -> (Label.to_string l1) = (Label.to_string l2)
  | Instr i1, Instr i2 -> _instr_equal i1 i2
  | _ -> false
;;


type block_info =
  { instrs : t list
  ; node   : Graph.node
  }

(* context to help build a cfg *)
type context =
  { next_block_id        : int
  ; curr_block_id        : int
  ; rev_curr_block_vasms : t list
  ; label_to_block_id    : (string, int) Map.t
  ; id_to_block_info     : (int, block_info) Map.t
  ; block_id_graph       : int Graph.t
  ; rev_finished_nodes   : Graph.node list (* in reverse order of finishing *)
  }

(* use and return [ctx.next_block_id]. NOTE won't change [ctx.curr_block_id] *)
let _ctx_allocate_new_block (ctx : context) : (context * int) =
  let new_block_id = ctx.next_block_id in
  let new_graph, node = Graph.create_node ctx.block_id_graph new_block_id in
  let block_info = { instrs = []; node } in
  let id_to_block_info = Map.add new_block_id block_info ctx.id_to_block_info in
  let ctx = { ctx with id_to_block_info;
                       next_block_id = new_block_id + 1;
                       block_id_graph = new_graph } in
  (ctx, new_block_id)
;;

let _ctx_get_block_info (ctx : context) (id : int) : block_info =
  match Map.get id ctx.id_to_block_info with
  | None -> failwith "[Vasm._ctx_get_block_info] Unbound id"
  | Some block_info -> block_info
;;

let _ctx_get_node (ctx : context) (id : int) : Graph.node =
  (_ctx_get_block_info ctx id).node
;;

(* Return node of starting block too. NO SIDE EFFECT *)
let _ctx_init : context =
  let ctx = { next_block_id        = 0
            ; curr_block_id        = -1 (* temporary garbage *)
            ; rev_curr_block_vasms = []
            ; label_to_block_id    = Map.empty String.compare
            ; id_to_block_info     = Map.empty Int.compare
            ; block_id_graph       = Graph.empty_graph () 
            ; rev_finished_nodes   = [] }
  in
  let ctx, start_id = _ctx_allocate_new_block ctx in
  { ctx with curr_block_id = start_id }
;;

let _ctx_add_block_info (ctx : context) (id : int) (info : block_info)
  : context =
  let id_to_block_info = Map.add id info ctx.id_to_block_info in
  { ctx with id_to_block_info }
;;

let _ctx_get_or_gen_label_id (ctx : context) (label : Label.t)
  : (context * int) =
  let label_str = Label.to_string label in
  match Map.get label_str ctx.label_to_block_id with
  | Some block_id -> (ctx, block_id)
  | None ->
    let ctx, block_id = _ctx_allocate_new_block ctx in
    let label_to_block_id = Map.add label_str block_id ctx.label_to_block_id in
    let ctx = { ctx with label_to_block_id } in
    (ctx, block_id)
;;

let _ctx_get_block_instrs (ctx : context) (id : int) : t list =
  (_ctx_get_block_info ctx id).instrs
;;

let _ctx_add_edge_btw_blocks (ctx : context) (src_id : int) (dst_id : int)
  : context =
  let src_node = _ctx_get_node ctx src_id in
  let dst_node = _ctx_get_node ctx dst_id in
  let block_id_graph = Graph.add_edge ctx.block_id_graph src_node dst_node in
  { ctx with block_id_graph }
;;

let _ctx_add_instr (ctx : context) t : context =
  let rev_curr_block_vasms = t::ctx.rev_curr_block_vasms in
  { ctx with rev_curr_block_vasms }
;;

(* won't update [next/curr_block_id] or [rev_curr_block_vasms] *)
let _ctx_finish_curr_block (ctx : context) : context =
  let block_instrs = List.rev ctx.rev_curr_block_vasms in
  let block_id = ctx.curr_block_id in
  let block_info = _ctx_get_block_info ctx block_id in
  let block_info = { block_info with instrs = block_instrs } in
  let id_to_block_info = Map.add block_id block_info ctx.id_to_block_info in
  let rev_finished_nodes = block_info.node::ctx.rev_finished_nodes in
  { ctx with id_to_block_info; rev_finished_nodes }
;;

let _ctx_start_block_with_label (ctx : context) (label : Label.t) : context =
  let ctx, new_block_id = _ctx_get_or_gen_label_id ctx label in
  { ctx with curr_block_id = new_block_id;
             rev_curr_block_vasms = [] }
;;

(* NOTE source of intermediate empty block. *)
let _ctx_start_block (ctx : context) : context =
  let ctx, new_id = _ctx_allocate_new_block ctx in
  { ctx with curr_block_id = new_id 
           ; rev_curr_block_vasms = [] }
;;

let _ctx_handle_label (ctx : context) (label : Label.t) : context =
  let old_block_id = ctx.curr_block_id in
  let ctx = _ctx_finish_curr_block ctx in
  let ctx = _ctx_start_block_with_label ctx label in
  let new_block_id = ctx.curr_block_id in
  let ctx = _ctx_add_edge_btw_blocks ctx old_block_id new_block_id in
  _ctx_add_instr ctx (Label label)
;;

(* ASSUME the ret instruction has been added to [ctx] *)
let _ctx_handle_ret (ctx : context) : context =
  let ctx = _ctx_finish_curr_block ctx in
  _ctx_start_block ctx
;;

let _ctx_handle_jump (ctx : context) (jump : jump) : context =
  let ctx, target_id = _ctx_get_or_gen_label_id ctx jump.target in
  let ctx = _ctx_add_edge_btw_blocks ctx ctx.curr_block_id target_id in
  let ctx = _ctx_finish_curr_block ctx in
  let old_block_id = ctx.curr_block_id in
  let ctx = _ctx_start_block ctx in
  match jump.kind with
  | Unconditional -> ctx
  | Conditional ->
    _ctx_add_edge_btw_blocks ctx old_block_id ctx.curr_block_id
;;

let _ctx_handle_instr (ctx : context) (instr : instr) : context =
  let ctx = _ctx_add_instr ctx (Instr instr) in
  match instr.desc with
  | Ret    -> _ctx_handle_ret ctx
  | Linear -> ctx (* linear control flow, i.e., straight to next instr *)
  | Jump jump -> _ctx_handle_jump ctx jump
;;

(* update [ctx] for wiring between nodes, starting new node, or adding [t] *)
let _ctx_handle_one_vasm (ctx : context) t : context =
  match t with
  | Label label -> _ctx_handle_label ctx label
  | Instr instr   -> _ctx_handle_instr ctx instr
;;

let rec _ctx_handle_vasms (ctx : context) (ts : t list) : context =
  match ts with
  | [] -> _ctx_finish_curr_block ctx
  | t::ts ->
    let ctx = _ctx_handle_one_vasm ctx t in
    _ctx_handle_vasms ctx ts
;;

(* not operating o [ctx] because I don't want to break its invariants *)
let _keep_only_finished_nodes_in_cfg
    (ctx : context) (cfg: 'a Graph.t) : 'a Graph.t =
  let all_node_set = Graph.get_all_nodes cfg in
  let unfinished_node_set =
    List.fold_right Set.remove ctx.rev_finished_nodes all_node_set
  in
  Set.fold
    (fun cfg node ->
       if Set.mem node unfinished_node_set
       then Graph.remove_node cfg node
       else cfg)
    cfg all_node_set
;;

let build_cfg vasms =
  let ctx = _ctx_init in
  let ctx = _ctx_handle_vasms ctx vasms in
  let cfg = _keep_only_finished_nodes_in_cfg ctx ctx.block_id_graph in
  let cfg = Graph.map (_ctx_get_block_instrs ctx) cfg in
  let finished_nodes = List.rev ctx.rev_finished_nodes in
  (cfg, finished_nodes)
;;


let _pp_jump (jump : jump) : string =
  let kind_str =
    match jump.kind with
    | Conditional -> "conditional"
    | Unconditional -> "unconditional"
  in
  String.concat [kind_str; " jump to "; Label.to_string jump.target]
;;

let _pp_instr (instr : instr) : string =
  let desc_str =
    match instr.desc with
    | Linear -> "instr"
    | Ret  -> "ret"
    | Jump jump -> _pp_jump jump
  in
  let rw_str =
    String.concat
      [", reads = "; Set.to_string Temp.to_string instr.reads;
       ", writes = "; Set.to_string Temp.to_string instr.writes;]
  in
  String.append desc_str rw_str
;;

let pp t =
  match t with
  | Label label -> Label.to_string label
  | Instr instr -> _pp_instr instr
;;
