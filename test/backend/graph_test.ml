open Pervasives

let _add_edges (graph : 'a Graph.t) (edges : (Graph.node * Graph.node) list)
  : 'a Graph.t =
  List.fold_left
    (fun graph (src_node, dst_node) ->
       Graph.add_edge graph src_node dst_node)
    graph edges
;;

let _check_edges
    (graph : 'a Graph.t) (edges : (Graph.node * Graph.node list) list) : unit =
  List.iter (* check out_neighbor of each src_node *)
    (fun (src_node, dst_nodes) ->
       Test_aux.check_set dst_nodes (Graph.get_out_neighbors graph src_node);
       List.iter (* check each in_neighbor of dst_node *)
         (fun dst_node ->
            OUnit2.assert_bool
              "edge should create in_neighbors too"
              (Set.mem src_node (Graph.get_in_neighbors graph dst_node)))
         dst_nodes)
    edges
;;

(* NOTE general structural comparison is used to check equality of annots *)
let _check_nodes_and_annots
    (graph : 'a Graph.t) (expected_annots : (Graph.node * 'a) list) : unit =
  let node_set = Graph.get_all_nodes graph in
  let expected_nodes = List.map (fun (node, _) -> node) expected_annots in
  Test_aux.check_set expected_nodes node_set;
  List.iter
    (fun (node, expected_annot) -> 
       let actual_annot = Graph.get_annot graph node in
       OUnit2.assert_equal expected_annot actual_annot)
    expected_annots
;;


let tests = OUnit2.(>:::) "graph_test" [

    OUnit2.(>::) "test_create_nodes" (fun _ ->
        let graph = Graph.empty_graph () in
        let graph, n1 = Graph.create_node graph 1 in
        let graph, n2 = Graph.create_node graph 2 in

        (* annotations should match what's given during creation
         * all and only created nodes should be in the graph *)
        _check_nodes_and_annots graph [(n1, 1); (n2, 2)];
      );

    OUnit2.(>::) "test_remove_nodes" (fun _ ->
        (* 1<--->2
         * |    /^
         * |   / |
         * v v   |
         * 3     4
         *)
        let graph = Graph.empty_graph () in
        let graph, n1 = Graph.create_node graph 1 in
        let graph, n2 = Graph.create_node graph 2 in
        let graph, n3 = Graph.create_node graph 3 in
        let graph, n4 = Graph.create_node graph 4 in
        let graph = _add_edges graph [(n1, n2); (n1, n3);
                                      (n2, n1); (n2, n3); (n4, n2)]
        in

        (* remove a node without in_neighbors
         *
         * 1<--->2
         * |    /
         * |   / 
         * v v   
         * 3     
         *)
        let graph = Graph.remove_node graph n4 in
        _check_nodes_and_annots graph [(n1, 1); (n2, 2); (n3, 3)];
        _check_edges graph [(n1, [n2; n3]); (n2, [n1; n3]); (n3, [])];

        (* remove a node without out_neighbors
         *
         * 1<--->2
         *)
        let graph = Graph.remove_node graph n3 in
        _check_nodes_and_annots graph [(n1, 1); (n2, 2)];
        _check_edges graph [(n1, [n2]); (n2, [n1])];

        (* remove a node with mutual connection
         *
         * 1
         *)
        let graph = Graph.remove_node graph n2 in
        _check_nodes_and_annots graph [(n1, 1)];
      );

    OUnit2.(>::) "test_get_set_annot" (fun _ ->
        let graph = Graph.empty_graph () in
        let graph, n1 = Graph.create_node graph 1 in
        let graph, n2 = Graph.create_node graph 2 in
        let graph = Graph.set_annot graph n1 11 in
        let graph = Graph.set_annot graph n2 22 in

        (* [set_annot] shouldn't change nodes themselves in the graph *)
        _check_nodes_and_annots graph [(n1, 11); (n2, 22)];
      );

    OUnit2.(>::) "test_add_edge" (fun _ ->
        (* 1 --> 2
         * |    ^
         * |   /
         * v v
         * 3 
         *)
        let graph = Graph.empty_graph () in
        let graph, n1 = Graph.create_node graph 1 in
        let graph, n2 = Graph.create_node graph 2 in
        let graph, n3 = Graph.create_node graph 3 in
        let graph = _add_edges graph [(n1, n2); (n1, n3); (n2, n3); (n3, n2)] in

        (* annotation or nodes shouldn't be changed after adding edges *)
        _check_nodes_and_annots graph [(n1, 1); (n2, 2); (n3, 3)];

        (* edges should be reflected via neighbor querying functions *)
        _check_edges graph [(n1, [n2; n3]); (n2, [n3]); (n3, [n2])]
      );

    OUnit2.(>::) "test_map" (fun _ ->
        let graph = Graph.empty_graph () in
        let graph, n1 = Graph.create_node graph 1 in
        let graph, n2 = Graph.create_node graph 2 in
        let graph = Graph.map Int.to_string graph in

        (* Previously created nodes should be usable in the mapped graph.
         * Nodes themselves should be unchanged in the mapped graph *)
        _check_nodes_and_annots graph [(n1, "1"); (n2, "2")];
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
