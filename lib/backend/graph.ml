open Pervasives

type node = int

let _empty_node_set = Set.empty Int.compare
;;


type 'a node_info =
  { out_nbs : node Set.t
  ; in_nbs  : node Set.t
  ; annot   : 'a
  }

type 'a t =
  { node_infos        : (node, 'a node_info) Map.t
  ; next_node         : node
  }


let empty_graph () =
  { node_infos    = Map.empty Int.compare
  ; next_node     = 0
  }
;;

let _get_node_info (t : 'a t) (n : node) (func : string) : 'a node_info =
  match Map.get n t.node_infos with
  | None ->
    failwith 
      (String.join_with ["[Graph."; func; "] node not bound in graph"] "")
  | Some info -> info
;;

let _check_node_exists t (n : node) (func : string) : unit =
  let _ = _get_node_info t n func in
  ()
;;

let _set_node_info (t : 'a t) (n : node) (info : 'a node_info) : 'a t =
  let node_infos = Map.add n info t.node_infos in
  { t with node_infos }
;;

let create_node t annot =
  let node = t.next_node in
  let info = { out_nbs = _empty_node_set
             ; in_nbs = _empty_node_set
             ; annot } in
  let t = _set_node_info t node info in
  let t = { t with next_node = node + 1 } in
  (t, node)
;;

let remove_node t node =
  (* NOTE [_get_node_info] disallows batch update, unless we add another
   * indirection, but KISS for now. *)
  let _remove_node_from_out_nbs_of t to_remove nodes : 'a t =
    Set.fold
      (fun t node ->
         let node_info = _get_node_info t node "remove_node" in
         let node_info = { node_info with
                           out_nbs = Set.remove to_remove node_info.out_nbs } in
         { t with node_infos = Map.add node node_info t.node_infos })
      t nodes
  in
  let _remove_node_from_in_nbs_of t to_remove nodes : 'a t =
    Set.fold
      (fun t node ->
         let node_info = _get_node_info t node "remove_node" in
         let node_info = { node_info with
                           in_nbs = Set.remove to_remove node_info.in_nbs } in
         { t with node_infos = Map.add node node_info t.node_infos })
      t nodes
  in
  (* must severe connections first then remove node, to maintain invariants *)
  let info = _get_node_info t node "remove_node" in
  let t = _remove_node_from_out_nbs_of t node info.in_nbs in
  let t = _remove_node_from_in_nbs_of t node info.out_nbs in
  { t with node_infos = Map.remove node t.node_infos }
;;

let get_annot t node =
  let info = _get_node_info t node "get_annot" in
  info.annot
;;

let set_annot t node annot =
  let info = _get_node_info t node "set_annot" in
  let new_info = { info with annot } in
  _set_node_info t node new_info
;;

let add_edge t src dst =
  let src_info = _get_node_info t src "add_edge" in
  let dst_info = _get_node_info t dst "add_edge" in
  let src_out_nbs = Set.add dst src_info.out_nbs in
  let dst_in_nbs  = Set.add src dst_info.in_nbs in
  let new_src_info = { src_info with out_nbs = src_out_nbs } in
  let new_dst_info = { dst_info with in_nbs = dst_in_nbs } in
  let t = _set_node_info t src new_src_info in
  _set_node_info t dst new_dst_info
;;

let get_out_neighbors t node =
  let info = _get_node_info t node "add_edge" in
  info.out_nbs
;;

let get_in_neighbors t node =
  let info = _get_node_info t node "add_edge" in
  info.in_nbs
;;

let map f t =
  let node_infos =
    Map.map
      (fun info -> 
         let new_annot = f info.annot in
         { info with annot = new_annot })
        t.node_infos
  in
  { t with node_infos }
;;

let get_all_nodes t =
  Map.get_key_set t.node_infos
;;
