
type t = Namer.t

let init =
  Namer.init "x"
;;

let rename_struct t structure =
  Namer.rename_vars_in_ast_struct t structure
;;

let gen_new_var_with_prefix =
  Namer.gen_new_name_with_prefix
;;
