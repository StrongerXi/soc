open Pervasives

let int_cmp n1 n2 =
  if n1 = n2 then 0
  else if n1 > n2 then 1
  else -1
;;

(** first element in [xs] will be added last *)
let from_list xs cmp =
  List.fold_right
    (fun (k, v) m -> Map.add k v m)
    xs
    (Map.empty cmp)
;;

let emp_int = Map.empty int_cmp
;;

let tests = OUnit2.(>:::) "map_tests" [

    (* NOTE [Map.add] and [Map.remove] are implicitly tested *)

    OUnit2.(>::) "test_is_empty" (fun _ ->
        OUnit2.assert_equal true (Map.is_empty emp_int);
        OUnit2.assert_equal false (Map.is_empty (Map.add 42 'a' emp_int));
      );

    OUnit2.(>::) "test_size" (fun _ ->
        let s2 = from_list [(42, 'a'); (1, 'b'); (42, 'c')] int_cmp in
        OUnit2.assert_equal 0 (Map.size emp_int);
        OUnit2.assert_equal 0 (Map.size (Map.remove 1 emp_int));
        OUnit2.assert_equal 2 (Map.size s2);
        OUnit2.assert_equal 1 (Map.size (Map.remove 42 s2));
      );

    OUnit2.(>::) "test_get" (fun _ ->
        let s2 = from_list [(42, 'a'); (1, 'b'); (42, 'c')] int_cmp in
        OUnit2.assert_equal None (Map.get 42 emp_int);
        OUnit2.assert_equal None (Map.get 2 s2);
        OUnit2.assert_equal (Some 'a') (Map.get 42 s2);
        OUnit2.assert_equal None (Map.get 42 (Map.remove 42 s2));
        OUnit2.assert_equal (Some 'b') (Map.get 1 (Map.remove 42 s2));
      );

    OUnit2.(>::) "test_map" (fun _ ->
        let dup n = (n, n) in
        let s2 = from_list [(42, 'a'); (1, 'b')] int_cmp in
        let s2 = Map.map dup s2 in
        OUnit2.assert_equal 0 (Map.size (Map.map dup emp_int));
        OUnit2.assert_equal 2 (Map.size s2);
        OUnit2.assert_equal (Some ('a', 'a')) (Map.get 42 s2);
        OUnit2.assert_equal (Some ('b', 'b')) (Map.get 1 s2);
      );

    OUnit2.(>::) "test_mapi" (fun _ ->
        let tup k v = (k, v) in
        let s2 = from_list [(42, 'a'); (1, 'b')] int_cmp in
        let s2 = Map.mapi tup s2 in
        OUnit2.assert_equal 0 (Map.size (Map.map tup emp_int));
        OUnit2.assert_equal 2 (Map.size s2);
        OUnit2.assert_equal (Some (42, 'a')) (Map.get 42 s2);
        OUnit2.assert_equal (Some (1, 'b')) (Map.get 1 s2);
      );

    OUnit2.(>::) "test_fold" (fun _ ->
        let s2 = from_list [(42, 'a'); (1, 'b')] int_cmp in
        let f v acc = v::acc in
        OUnit2.assert_equal [] (Map.fold f emp_int []);
        let res = Map.fold f s2 [] in
        OUnit2.assert_equal 2 (List.length res);
        OUnit2.assert_equal true (List.mem 'a' res);
        OUnit2.assert_equal true (List.mem 'b' res);
      );

    OUnit2.(>::) "test_foldi" (fun _ ->
        let s2 = from_list [(42, 'a'); (1, 'b')] int_cmp in
        let f k v acc = (k, v)::acc in
        OUnit2.assert_equal [] (Map.foldi f emp_int []);
        let res = Map.foldi f s2 [] in
        OUnit2.assert_equal 2 (List.length res);
        OUnit2.assert_equal true (List.mem (42, 'a') res);
        OUnit2.assert_equal true (List.mem (1, 'b') res);
      );

    OUnit2.(>::) "test_to_string" (fun _ ->
        let s2 = from_list [(42, 'a'); (1, 'b')] int_cmp in
        OUnit2.assert_equal "{}"
          (Map.to_string Int.to_string Char.to_string emp_int);
        OUnit2.assert_equal "{(42, a); (1, b)}"
          (Map.to_string Int.to_string Char.to_string s2);
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
