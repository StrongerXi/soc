
(* functions to format datatypes from various parts of the compiler *)

(* Frontend stuff *)
val pp_token            : Token.t -> string
val pp_token_desc       : Token.desc -> string
val pp_ast_structure    : Ast.structure -> string
val pp_ast_expr         : Ast.expression -> string
val pp_ast_typ          : Ast.typ -> string

val pp_ast_structure_with_typ_annot : Ast.structure -> string
val pp_ast_expr_with_typ_annot      : Ast.expression -> string

val pp_lexer_error      : Errors.lexer_error -> string
val pp_parser_error     : Errors.parser_error -> string
val pp_ast_interp_error : Errors.ast_interp_error -> string
val pp_typer_error      : Errors.typer_error -> string

(* Backend stuff *)
val pp_cir : Cir.prog -> string
