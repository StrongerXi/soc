open Pervasives

(* TODO
 * - add optional trace info (string output for each completed stage)
 * - add config (that's probably much later) *)

let lex_file filepath =
  let content = Io.read_file filepath in
  match Lexer.lex content with
  | Error err -> Error (Pretty.pp_lexer_error err)
  | Ok tokens -> Ok tokens
;;

let parse_file filepath =
  let parse_tokens tokens =
    match Parser.parse tokens with
    | Error err -> Error (Pretty.pp_parser_error err)
    | Ok ast -> Ok ast
  in
  Result.bind (lex_file filepath) parse_tokens
;;

let type_file filepath =
  let type_ast ast =
    match Typer.type_struct ast with
    | Error errs ->
      Error (String.join_with (List.map Pretty.pp_typer_error errs) "\n")
    | Ok ast -> Ok ast
  in
  Result.bind (parse_file filepath) type_ast
;;

let cir_file filepath =
  Result.map Cir.from_ast_struct (type_file filepath)
;;

let lir_file filepath =
  Result.map Lir.from_cir_prog (cir_file filepath)
;;


let rec _x86_temp_func_to_func (temp_func : X86.temp_func) : X86.func =
  let vasms = X86.temp_func_to_vasms temp_func in
  let annotated_vasms = Liveness_analysis.analyze_vasm vasms in
  let pre_color = X86.get_pre_coloring temp_func in
  match Reg_alloc.greedy_alloc
          annotated_vasms 
          X86.assignable_regs
          pre_color with
  | Ok temp_to_reg -> X86.temp_func_to_func temp_func temp_to_reg 
  | Error temps_to_spill ->
    let updated_temp_func = X86.spill_temps temp_func temps_to_spill in
    _x86_temp_func_to_func updated_temp_func
;;

let lir_to_x86 lir_prog = 
  let x86_temp_prog = X86.from_lir_prog lir_prog in
  { X86.funcs = List.map _x86_temp_func_to_func x86_temp_prog.temp_funcs
  ; main      = _x86_temp_func_to_func x86_temp_prog.temp_main
  }
;;

