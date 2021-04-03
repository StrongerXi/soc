open Pervasives

let tests = OUnit2.(>:::) "substs_test" [

    (* NOTE substs application is implicitly tested too *)
    OUnit2.(>::) "test_unify" (fun _ ->
        let substs = Substs.empty in
        (* [X -> int = int -> Y] ==> {X = int, Y = int} *)
        let ty1 = Ast.Typ_arrow (Ast.Typ_var (Some "X"), Ast.Typ_const "int") in
        let ty2 = Ast.Typ_arrow (Ast.Typ_const "int", Ast.Typ_var (Some "Y")) in
        let substs, opt_err = Substs.unify substs ty1 ty2 in
        OUnit2.assert_equal None opt_err;
        (* X -> Y ==> int -> int*)
        let ty3 = Ast.Typ_arrow (Ast.Typ_var (Some "X"), Ast.Typ_var (Some "Y")) in
        OUnit2.assert_equal
          (Substs.apply_to_typ substs ty3)
          (Ast.Typ_arrow (Ast.Typ_const "int", Ast.Typ_const "int"));
        (* A -> bool ==> A -> bool *)
        let ty4 = Ast.Typ_arrow (Ast.Typ_var (Some "A"), Ast.Typ_const "bool") in
        OUnit2.assert_equal (Substs.apply_to_typ substs ty4) ty4;
      );

    OUnit2.(>::) "test_unify_err" (fun _ ->
        let substs = Substs.empty in
        (* [X -> (Y -> bool) = int -> Y]. Y occurs in (Y -> bool) *)
        let ty1 = Ast.Typ_arrow (Ast.Typ_var (Some "Y"), Ast.Typ_const "bool") in
        let ty2 = Ast.Typ_arrow (Ast.Typ_var (Some "X"), ty1) in
        let ty3 = Ast.Typ_arrow (Ast.Typ_const "int", Ast.Typ_var (Some "Y")) in
        let substs, opt_err = Substs.unify substs ty2 ty3 in
        let occurs_err = Some (Substs.Unify_occurs ("Y", ty1)) in
        OUnit2.assert_equal occurs_err opt_err;
        (* {X = int} should be the current progress before occurs happened,
           [int -> bool = Y -> X] leads to mismatch error *)
        let ty1 = Ast.Typ_arrow (Ast.Typ_const "int", Ast.Typ_const "bool") in
        let ty2 = Ast.Typ_arrow (Ast.Typ_var (Some "Y"), Ast.Typ_var (Some "X")) in
        let _, opt_err = Substs.unify substs ty1 ty2 in
        let mismatch_err = Some Substs.Unify_mismatch in
        OUnit2.assert_equal mismatch_err opt_err;
      );

    OUnit2.(>::) "test_apply" (fun _ ->
        let substs = Substs.empty in
        (* empty substitution has no effect *)
        let ty1 = Ast.Typ_arrow (Ast.Typ_var (Some "X"), Ast.Typ_const "int") in
        let ty2 = Ast.Typ_arrow (Ast.Typ_const "int", Ast.Typ_var (Some "Y")) in
        OUnit2.assert_equal (Substs.apply_to_typ substs ty1) ty1;
        OUnit2.assert_equal (Substs.apply_to_typ substs ty2) ty2;
        (* [X -> int = int -> Y], [Z = bool] *)
        let substs, opt_err = Substs.unify substs ty1 ty2 in
        OUnit2.assert_equal None opt_err;
        let substs, opt_err =
          Substs.unify substs (Ast.Typ_var (Some "Z")) (Ast.Typ_const "boo") in
        OUnit2.assert_equal None opt_err;
        (* ignore X and Z when applying to (X -> Z) -> (Y -> Z) *)
        let ty3 = Ast.Typ_arrow (Ast.Typ_var (Some "X"), Ast.Typ_var (Some "Z")) in
        let ty4 = Ast.Typ_arrow (Ast.Typ_var (Some "Y"), Ast.Typ_var (Some "Z")) in
        let ty5 = Ast.Typ_arrow (ty3, ty4) in
        let to_ignore = ["X"; "Z"] in
        OUnit2.assert_equal
          (Substs.apply_to_typ_exclude substs ty5 to_ignore)
          (Ast.Typ_arrow
             (ty3, Ast.Typ_arrow (Ast.Typ_const "int", Ast.Typ_var (Some "Z"))));
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests
