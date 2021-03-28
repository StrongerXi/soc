open Pervasives

let tests = OUnit2.(>:::) "list_tests" [
    OUnit2.(>::) "test_cons" (fun _ ->
        OUnit2.assert_equal [1] (List.cons 1 []);
        OUnit2.assert_equal [1; 2; 3] (List.cons 1 [2; 3]);
      );

    OUnit2.(>::) "test_length" (fun _ ->
        OUnit2.assert_equal 0 (List.length []);
        OUnit2.assert_equal 1 (List.length [3]);
        OUnit2.assert_equal 3 (List.length [["foo"]; []; ["bar"; "baz"]]);
      );

    OUnit2.(>::) "test_rev" (fun _ ->
        OUnit2.assert_equal [] (List.rev []);
        OUnit2.assert_equal [["foo"]] (List.rev [["foo"]]);
        OUnit2.assert_equal [[2; 3]; []; [1]] (List.rev [[1]; []; [2; 3]]);
      );


    OUnit2.(>::) "test_append" (fun _ ->
        OUnit2.assert_equal [] (List.append [] []);
        OUnit2.assert_equal [1] (List.append [] [1]);
        OUnit2.assert_equal [1] (List.append [1] []);
        OUnit2.assert_equal [1; 2; 3; 4] (List.append [1; 2] [3; 4]);
      );
    
    OUnit2.(>::) "test_concat" (fun _ ->
        OUnit2.assert_equal [] (List.concat []);
        OUnit2.assert_equal [1; 3; 5] (List.concat [[1]; []; [3; 5]])
      );
    
    OUnit2.(>::) "test_flatten" (fun _ ->
        OUnit2.assert_equal [] (List.flatten []);
        OUnit2.assert_equal [1; 3; 5] (List.flatten [[1]; []; [3; 5]])
      );
    
    OUnit2.(>::) "test_map" (fun _ ->
        let double = fun n -> n + n in
        OUnit2.assert_equal [] (List.map double []);
        OUnit2.assert_equal [2; 6; 10] (List.map double [1; 3; 5]);
        let x = ref "" in
        let append n = x := (String.append !x (Int.to_string n)) in
        let _ = List.map append [1; 3; 5] in
        OUnit2.assert_equal "135" !x;
      );

    OUnit2.(>::) "test_iter" (fun _ ->
        let x = ref "" in
        let append n = x := (String.append !x (Int.to_string n)) in
        let _ = List.map append [1; 3; 5] in
        OUnit2.assert_equal "135" !x;
      );
    
    OUnit2.(>::) "test_fold_left" (fun _ ->
        let flip_cons = fun xs x -> x::xs in
        OUnit2.assert_equal [] (List.fold_left flip_cons [] []);
        OUnit2.assert_equal [3; 2; 1; 4;] (List.fold_left flip_cons [1; 4] [2; 3]);
      );
    
    OUnit2.(>::) "test_fold_right" (fun _ ->
        OUnit2.assert_equal [] (List.fold_right List.cons [] []);
        OUnit2.assert_equal [1; 4; 2; 3] (List.fold_right List.cons [1; 4] [2; 3]);
      );
    
    OUnit2.(>::) "test_for_all" (fun _ ->
        let is_one = fun x -> x = 1 in
        OUnit2.assert_equal true (List.for_all is_one []);
        OUnit2.assert_equal false (List.for_all is_one [1; 0; 1]);
        OUnit2.assert_equal true (List.for_all is_one [1; 1; 1]);
      );
    
    OUnit2.(>::) "test_exists" (fun _ ->
        let is_two = fun x -> x = 2 in
        OUnit2.assert_equal false (List.exists is_two []);
        OUnit2.assert_equal true (List.exists is_two [0; 2; 0]);
        OUnit2.assert_equal false (List.exists is_two [0; 1; 0]);
      );
    
    OUnit2.(>::) "test_mem" (fun _ ->
        OUnit2.assert_equal true (List.mem 0 [0; 1; 0]);
        OUnit2.assert_equal true (List.mem 1 [0; 1; 0]);
        OUnit2.assert_equal false (List.mem 2 [0; 1; 0]);
      );
    
    OUnit2.(>::) "test_find_opt" (fun _ ->
        let is_two = fun x -> x = 2 in
        OUnit2.assert_equal None (List.find_opt is_two []);
        OUnit2.assert_equal (Some 2) (List.find_opt is_two [0; 2; 0]);
        OUnit2.assert_equal None (List.find_opt is_two [0; 1; 0]);
      );
    
    OUnit2.(>::) "test_filter" (fun _ ->
        let is_two = fun x -> x = 2 in
        OUnit2.assert_equal [] (List.filter is_two []);
        OUnit2.assert_equal [2; 2] (List.filter is_two [0; 2; 2; 0]);
        OUnit2.assert_equal [] (List.filter is_two [0; 1; 0]);
      );
    
    OUnit2.(>::) "test_partition" (fun _ ->
        let is_positive = fun x -> x > 0 in
        OUnit2.assert_equal ([], []) (List.partition is_positive []);
        OUnit2.assert_equal
          ([1; 3], [-2; 0])
          (List.partition is_positive [1; -2; 0; 3]);
      );
    
    OUnit2.(>::) "test_assoc_opt" (fun _ ->
        OUnit2.assert_equal None (List.assoc_opt 2 []);
        OUnit2.assert_equal
          (Some "foo")
          (List.assoc_opt 2 [(1, "bar"); (2, "foo"); (3, "baz")]);
      );
    
    OUnit2.(>::) "test_remove_assoc" (fun _ ->
        OUnit2.assert_equal [] (List.remove_assoc 2 []);
        OUnit2.assert_equal
          [(1, "bar"); (3, "baz")]
          (List.remove_assoc 2 [(1, "bar"); (2, "foo"); (3, "baz")]);
      );

    OUnit2.(>::) "test_remove_dups" (fun _ ->
        let are_dups (x1, _) (x2, _) = x1 = x2 in
        OUnit2.assert_equal [] (List.remove_dups are_dups []);
        OUnit2.assert_equal
          [(1, 5); (2, 5)]
          (List.remove_dups are_dups [(1, 5); (1, 3); (2, 5); (2, 7)])
      );
    
    OUnit2.(>::) "test_split" (fun _ ->
        OUnit2.assert_equal ([], []) (List.split []);
        OUnit2.assert_equal
          ([1; 2], ["a"; "b"])
          (List.split [(1, "a"); (2, "b")]);
      );

    OUnit2.(>::) "test_combine" (fun _ ->
        OUnit2.assert_equal [] (List.combine [] []);
        OUnit2.assert_equal [] (List.combine [] [2; 4]);
        OUnit2.assert_equal [] (List.combine [4] []);
        OUnit2.assert_equal [(1, 4); (2, 5)] (List.combine [1; 2] [4; 5]);
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
