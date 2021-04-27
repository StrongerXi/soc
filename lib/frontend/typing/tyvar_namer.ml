
type t = Namer.t

let init =
  Namer.init "X"
;;

let gen_new_tyvar t : (t * Ast.typ) =
  let t, name = Namer.gen_new_name t in
  let tyvar = Ast.Typ_var (Some name) in
  (t, tyvar)
;;

let rename_struct t structure =
  Namer.rename_tyvars_in_ast_struct t structure
;;
