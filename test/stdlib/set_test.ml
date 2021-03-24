open Pervasives

let int_cmp n1 n2 =
  if n1 = n2 then 0
  else if n1 > n2 then 1
  else -1
;;

let from_list xs cmp =
  List.fold_right Set.add xs (Set.empty cmp)
;;

let emp_int = Set.empty int_cmp
;;

let tests = OUnit2.(>:::) "set_tests" [

    (* NOTE [Set.add] and [Set.remove] are implicitly tested *)

    OUnit2.(>::) "test_is_empty" (fun _ ->
        OUnit2.assert_equal true (Set.is_empty emp_int);
        OUnit2.assert_equal false (Set.is_empty (Set.add 42 emp_int));
      );

    OUnit2.(>::) "test_size" (fun _ ->
        let s2 = from_list [42; 1; 42] int_cmp in
        OUnit2.assert_equal 0 (Set.size emp_int);
        OUnit2.assert_equal 0 (Set.size (Set.remove 1 emp_int));
        OUnit2.assert_equal 2 (Set.size s2);
        OUnit2.assert_equal 1 (Set.size (Set.remove 42 s2));
      );

    OUnit2.(>::) "test_mem" (fun _ ->
        let s2 = from_list [42; 1; 42] int_cmp in
        OUnit2.assert_equal false (Set.mem 42 emp_int);
        OUnit2.assert_equal false (Set.mem 2 s2);
        OUnit2.assert_equal true (Set.mem 42 s2);
        OUnit2.assert_equal false (Set.mem 42 (Set.remove 42 s2));
        OUnit2.assert_equal true (Set.mem 1 (Set.remove 42 s2));
      );

    OUnit2.(>::) "test_map" (fun _ ->
        let double n = n + n in
        let s2 = from_list [1; 42] int_cmp in
        let s2 = Set.map double s2 in
        OUnit2.assert_equal 0 (Set.size (Set.map double emp_int));
        OUnit2.assert_equal 2 (Set.size s2);
        OUnit2.assert_equal false (Set.mem 1 s2);
        OUnit2.assert_equal true (Set.mem 2 s2);
        OUnit2.assert_equal false (Set.mem 42 s2);
        OUnit2.assert_equal true (Set.mem 84 s2);
      );

    OUnit2.(>::) "test_union" (fun _ ->
        let s2 = from_list [1; 42] int_cmp in
        let s3 = from_list [42; 5; 7;] int_cmp in
        let s4 = Set.union s2 s3 in
        OUnit2.assert_equal 0 (Set.size (Set.union emp_int emp_int));
        OUnit2.assert_equal 3 (Set.size (Set.union s3 emp_int));
        OUnit2.assert_equal 2 (Set.size (Set.union emp_int s2));
        OUnit2.assert_equal 4 (Set.size s4);
        OUnit2.assert_equal true (Set.mem 1 s4);
        OUnit2.assert_equal true (Set.mem 5 s4);
        OUnit2.assert_equal true (Set.mem 7 s4);
        OUnit2.assert_equal true (Set.mem 42 s4);
        OUnit2.assert_equal false (Set.mem 0 s4);
      );

    OUnit2.(>::) "test_inter" (fun _ ->
        let s4 = from_list [33; 11; 1; 42] int_cmp in
        let s3 = from_list [42; 7; 11;] int_cmp in
        let s2 = Set.inter s4 s3 in
        OUnit2.assert_equal 0 (Set.size (Set.inter emp_int emp_int));
        OUnit2.assert_equal 0 (Set.size (Set.inter s3 emp_int));
        OUnit2.assert_equal 0 (Set.size (Set.inter emp_int s2));
        OUnit2.assert_equal 2 (Set.size s2);
        OUnit2.assert_equal true (Set.mem 11 s2);
        OUnit2.assert_equal true (Set.mem 42 s2);
        OUnit2.assert_equal false (Set.mem 1 s2);
        OUnit2.assert_equal false (Set.mem 7 s2);
        OUnit2.assert_equal false (Set.mem 33 s2);
      );

    OUnit2.(>::) "test_disjoint" (fun _ ->
        let s4 = from_list [33; 11; 1; 42] int_cmp in
        let s3 = from_list [42; 7; 11;] int_cmp in
        let s3' = from_list [12; 0; 8;] int_cmp in
        OUnit2.assert_equal true (Set.disjoint emp_int emp_int);
        OUnit2.assert_equal true (Set.disjoint s3 emp_int);
        OUnit2.assert_equal true (Set.disjoint emp_int s4);
        OUnit2.assert_equal false (Set.disjoint s4 s3);
        OUnit2.assert_equal false (Set.disjoint s3 s4);
        OUnit2.assert_equal false (Set.disjoint s3 s3);
        OUnit2.assert_equal false (Set.disjoint s4 s4);
        OUnit2.assert_equal true (Set.disjoint s3' s3);
        OUnit2.assert_equal true (Set.disjoint s3' s4);
        OUnit2.assert_equal true (Set.disjoint s3 s3');
        OUnit2.assert_equal true (Set.disjoint s4 s3');
      );

    OUnit2.(>::) "test_diff" (fun _ ->
        let s4 = from_list [33; 11; 1; 42] int_cmp in
        let s3 = from_list [42; 7; 11;] int_cmp in
        OUnit2.assert_equal 0 (Set.size (Set.diff emp_int emp_int));
        OUnit2.assert_equal 3 (Set.size (Set.diff s3 emp_int));
        OUnit2.assert_equal 0 (Set.size (Set.diff emp_int s4));

        let s1 = Set.diff s3 s4 in
        OUnit2.assert_equal 1 (Set.size s1);
        OUnit2.assert_equal true (Set.mem 7 s1);
        OUnit2.assert_equal false (Set.mem 42 s1);
        OUnit2.assert_equal false (Set.mem 11 s1);
        OUnit2.assert_equal false (Set.mem 33 s1);
        OUnit2.assert_equal false (Set.mem 1 s1);

        let s2 = Set.diff s4 s3 in
        OUnit2.assert_equal 2 (Set.size s2);
        OUnit2.assert_equal true (Set.mem 33 s2);
        OUnit2.assert_equal true (Set.mem 1 s2);
        OUnit2.assert_equal false (Set.mem 7 s2);
        OUnit2.assert_equal false (Set.mem 42 s2);
        OUnit2.assert_equal false (Set.mem 11 s2);
      );

    OUnit2.(>::) "test_subset" (fun _ ->
        let s4 = from_list [33; 11; 1; 42] int_cmp in
        let s3 = from_list [42; 7; 11;] int_cmp in
        let s1 = from_list [11] int_cmp in
        OUnit2.assert_equal true (Set.subset emp_int emp_int);
        OUnit2.assert_equal true (Set.subset emp_int s4);
        OUnit2.assert_equal false (Set.subset s3 emp_int);
        OUnit2.assert_equal false (Set.subset s4 s3);
        OUnit2.assert_equal false (Set.subset s3 s4);
        OUnit2.assert_equal true (Set.subset s3 s3);
        OUnit2.assert_equal true (Set.subset s4 s4);
        OUnit2.assert_equal true (Set.subset s1 s3);
        OUnit2.assert_equal true (Set.subset s1 s4);
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
