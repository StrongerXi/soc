
(** [interp_struct ctx struct] interprets [struct] in [ctx].
    Supported native functions:
    - [println e] prints out result of each [e] on newline and returns it. *)
val interp_struct : Ast.structure -> unit
