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

    OUnit2.(>::) "test_add_list" (fun _ ->
        let s2 = from_list [1; 2] int_cmp in
        Test_aux.check_set [] (Set.add_list [] emp_int);
        Test_aux.check_set [1; 2] (Set.add_list [1; 2] emp_int);
        Test_aux.check_set [1; 2; 3; 4] (Set.add_list [3; 4] s2);
      );

    OUnit2.(>::) "test_get_one" (fun _ ->
        let _check_get_one (set : int Set.t) : unit =
          match Set.get_one set with
          | None ->
            OUnit2.assert_bool
              "Non-empty set returned nothing"
              ((Set.size set) = 0)
          | Some n ->
            OUnit2.assert_bool
              "get_one should return a member of set"
              (Set.mem n set)
        in
        let s1 = from_list [42] int_cmp in
        let s2 = from_list [1; 42] int_cmp in
        _check_get_one emp_int;
        _check_get_one s1;
        _check_get_one s2;
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
        Test_aux.check_set [] (Set.map double emp_int);
        Test_aux.check_set [2; 84] (Set.map double s2);
      );

    OUnit2.(>::) "test_filter" (fun _ ->
        let s = from_list [1; 42; 22; 25] int_cmp in
        let even_s = Set.filter (fun n -> n mod 2 = 0) s in 
        Test_aux.check_set [42; 22] even_s;
      );

    OUnit2.(>::) "test_find" (fun _ ->
        let s = from_list [42; 1; 22; 25] int_cmp in
        OUnit2.assert_equal None (Set.find (fun n -> n = 2) s);
        OUnit2.assert_equal (Some 1) (Set.find (fun n -> n = 1) s);
        OUnit2.assert_equal (Some 22) (Set.find (fun n -> n = 22) s);
      );

    OUnit2.(>::) "test_fold" (fun _ ->
        let s = from_list [1; 42; 11] int_cmp in
        Test_aux.check_unordered_list
          [1; 42; 11]
          (Set.fold (fun l x -> x::l) [] s)
      );

    OUnit2.(>::) "test_union" (fun _ ->
        let s2 = from_list [1; 42] int_cmp in
        let s3 = from_list [42; 5; 7;] int_cmp in
        Test_aux.check_set [] (Set.union emp_int emp_int);
        Test_aux.check_set [42; 5; 7] (Set.union s3 emp_int);
        Test_aux.check_set [42; 1] (Set.union emp_int s2);
        Test_aux.check_set [1; 42; 5; 7] (Set.union s2 s3);
      );

    OUnit2.(>::) "test_inter" (fun _ ->
        let s4 = from_list [33; 11; 1; 42] int_cmp in
        let s3 = from_list [42; 7; 11;] int_cmp in
        Test_aux.check_set [] (Set.inter emp_int emp_int);
        Test_aux.check_set [] (Set.inter s3 emp_int);
        Test_aux.check_set [] (Set.inter emp_int s4);
        Test_aux.check_set [11; 42] (Set.inter s4 s3);
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
        Test_aux.check_set []          (Set.diff emp_int emp_int);
        Test_aux.check_set [42; 7; 11] (Set.diff s3 emp_int);
        Test_aux.check_set []          (Set.diff emp_int s4);
        Test_aux.check_set [7]         (Set.diff s3 s4);
        Test_aux.check_set [33; 1]     (Set.diff s4 s3);
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

    OUnit2.(>::) "test_to_list" (fun _ ->
        let s3 = from_list [11; 11; 1; 42; 1; 42] int_cmp in
        Test_aux.check_unordered_list [] (Set.to_list emp_int);
        Test_aux.check_unordered_list [11; 1; 42] (Set.to_list s3);
      );

    OUnit2.(>::) "test_to_string" (fun _ ->
        let s4 = from_list [33; 11; 1; 42] int_cmp in
        OUnit2.assert_equal "{}" (Set.to_string Int.to_string emp_int);
        OUnit2.assert_equal "{33; 11; 1; 42}" (Set.to_string Int.to_string s4);
      );

    OUnit2.(>::) "test_get_compare_func" (fun _ ->
        let lt = Int.compare in
        let gt = (fun n1 n2 -> -(Int.compare n1 n2)) in
        let slt = from_list [33; 11; 1; 42] lt in
        let sgt = from_list [33; 11; 1; 42] gt in
        let slt_func = Set.get_compare_func slt in
        let sgt_func = Set.get_compare_func sgt in
        OUnit2.assert_equal true ((slt_func 0 2) < 0);
        OUnit2.assert_equal true ((slt_func 1 1) = 0);
        OUnit2.assert_equal true ((slt_func 3 1) > 0);
        OUnit2.assert_equal true ((sgt_func 3 1) < 0);
        OUnit2.assert_equal true ((sgt_func 1 1) = 0);
        OUnit2.assert_equal true ((sgt_func 0 2) > 0);
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
