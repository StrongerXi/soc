
(** [rename_struct s] returns a new structure where 
    - variable names uniquely determines bindings, i.e., no more shadowing.
    - unbound names are left untouched (for better error reporting)
    - for duplicated names in the same scope, the last definition wins out.
    e.g.,
        let f x x = x in
        let f = k in
        x
        -------->
        let f1 x2 x3 = x3 in
        let f4 = k in
        x2
*)
val rename_struct : Ast.structure -> Ast.structure
