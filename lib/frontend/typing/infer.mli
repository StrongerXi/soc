open Pervasives

(** [infer_struct] will either return a list of errors from type inference, or
    ensures that in the output structure:
    - all variables are lexically bound
    - types at lhs of let are converted into principal form based on
      Hindley-Milner system
    - the program is free from static type error
    The function tries to collect as many errors as possible, e.g., [42 + false]
    results in error, but will be assumed to have type [int]. *)
val infer_struct : Ast.structure -> (Ast.structure, Errors.infer_error list) result
