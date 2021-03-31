open Pervasives

(** [infer_struct] will either return a list of errors from type inference, or
    ensures that in the output structure:
    - all variables are lexically bound
    - all types are converted into principal form based on Hindley-Milner system
    - the program is free from static type error *)
val infer_struct : Ast.structure -> (Ast.structure, Errors.infer_error list) result
