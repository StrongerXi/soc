open Pervasives

(* Again, this deserves a module but to simplify self-compilation... TODO *)
type printer = 
  { mutable space_level : int  (* # of spaces *)
  ; mutable buffer       : string
  ; mutable is_newline   : bool (* are we at beginning of a new line? *)
  }


let _create_printer () =
  { space_level = 0; buffer = ""; is_newline = true
  }
;;

let _inc_space (p : printer) (n : int) : unit =
  p.space_level <- p.space_level + n
;;

let _dec_space (p : printer) (n : int) : unit =
  p.space_level <- p.space_level - n
;;

let __print_str_only (p : printer) (s : string) : unit =
  p.buffer <- String.append p.buffer s
;;

(* [endline] specifies whether a newline will be printed to end current line *)
let __print_str (p : printer) (s : string) (endline : bool) : unit =
  let rec print_space n_more =
    if n_more = 0 then ()
    else (__print_str_only p " ";
          print_space (n_more - 1))
  in
  if p.is_newline
  then print_space p.space_level;
  __print_str_only p s;
  p.is_newline <- endline;
  if endline then __print_str_only p "\n";
;;

let _print_str (p : printer) (s : string) : unit =
  __print_str p s false
;;

let _println_str (p : printer) (s : string) : unit =
  __print_str p s true
;;

(* Nothing in between *)
let _print_strs (p : printer) (s : string list) : unit =
  List.iter (_print_str p) s
;;

let _print_newline (p : printer) : unit =
  __print_str_only p "\n";
  p.is_newline <- true;
;;

(* Again, this deserves a module but to simplify self-compilation... TODO *)


let _pp_lexer_action (what : Errors.lexer_action) : string =
  match what with
  | Lexing_expecting expect ->
    let reason = String.append "expecting '" (Char.to_string expect) in
    String.append reason "'"
  | Lexing_number -> "lexing a number"
  | Lexing_identifier_or_keyword -> "lexing an identifier or keyword"
;;

let pp_lexer_error (err : Errors.lexer_error) : string =
  match err with
  | Lexer_unexpected_char (expect, actual, loc) ->
    let msg = String.append "Expected '" (Char.to_string expect) in
    let msg = String.append msg "', but got '" in
    let msg = String.append msg (Char.to_string actual) in
    let msg = String.append msg "'" in
    let msg = String.append msg " at " in
    String.append msg (Location.to_string loc)
  | Lexer_unexpected_eof (where, action) ->
    let msg = "Unexpected EOF while " in
    let msg = String.append msg (_pp_lexer_action action) in
    let msg = String.append msg " at " in
    String.append msg (Location.to_string where)
  | Lexer_invalid_start (start, loc) ->
    let msg = "Invalid start of token: '" in
    let msg = String.append msg (Char.to_string start) in
    let msg = String.append msg "' at " in
    String.append msg (Location.to_string loc)
;;


let _pp_span (span : Span.t) : string =
  let str = String.append "<" (Location.to_string span.start) in
  let str = String.append str "-" in
  let str = String.append str (Location.to_string span.final) in
  String.append str ">"
;;


let pp_token_desc desc =
  match desc with
  | Token.Plus -> "<Plus>"
  | Token.Minus -> "<Minus>"
  | Token.Asterisk -> "<Asterisk>"
  | Token.AmperAmper -> "<AmperAmper>"
  | Token.BarBar -> "<BarBar>"
  | Token.If -> "<If>"
  | Token.Then -> "<Then>"
  | Token.Else -> "<Else>"
  | Token.Let -> "<Let>"
  | Token.Rec -> "<Rec>"
  | Token.Colon -> "<Colon>"
  | Token.Equal -> "<Equal>"
  | Token.And -> "<And>"
  | Token.In -> "<In>"
  | Token.Lparen -> "<Lparen>"
  | Token.Rparen -> "<Rparen>"
  | Token.Rarrow -> "<Rarrow>"
  | Token.Fun -> "<Fun>"
  | Token.Less -> "<Less>"
  | Token.True -> "<True>"
  | Token.False -> "<False>"
  | Token.Int s -> String.append (String.append "<Int (" s) ")>"
  | Token.DecapIdent s -> String.append (String.append "<DecapIdent (" s) ")>"
  | Token.SemiSemiColon -> "<SemiSemiColon>"
;;

let pp_token (tok : Token.t) =
  let str = String.append "{ " (pp_token_desc tok.token_desc) in
  let str = String.append str " in <" in
  let str = String.append str tok.token_span.filename in
  let str = String.append str ":" in
  let str = String.append str (_pp_span tok.token_span) in
  let str = String.append str "> }" in
  str
;;

let pp_parser_error (err : Errors.parser_error) =
  let pp_expected_tokens (toks : Token.desc list) : string =
    match toks with
    | [] -> "<unknown>"
    | _ ->
      let inner = String.join_with (List.map pp_token_desc toks) "; " in
      String.append "[" (String.append inner "]")
  in
  match err with
  | Parser_invalid_integer (text, span) ->
    let str = String.append "[Parser]: Invalid integer token <" text in
    let str = String.append str "> at " in
    let str = String.append str (_pp_span span) in
    String.append str "\n"

  | Parser_unexpected_token (actual, expects) ->
    let str = "[Parser]: Unexpected token " in
    let str = String.append str (pp_token actual) in
    let str = String.append str " where expected tokens are " in
    let str = String.append str (pp_expected_tokens expects) in
    String.append str "\n"

  | Parser_unexpected_eof (eof_loc, expects) ->
    let str = "[Parser]: Unexpected EOF at " in
    let str = String.append str (Location.to_string eof_loc) in
    let str = String.append str " where expected tokens are " in
    let str = String.append str (pp_expected_tokens expects) in
    String.append str "\n"
;;


let _is_atomic_typ (typ : Ast.typ) : bool =
  match typ.typ_desc with
  | Typ_const _ -> true
  | _ -> false
;;

let rec _pp_ast_typ (p : printer) (typ : Ast.typ) : unit =
  match typ.typ_desc with
  | Typ_const name -> _print_str p name;
  | Typ_arrow (in_ty, out_ty) ->
    if _is_atomic_typ in_ty
    then _pp_ast_typ p in_ty
    else (_print_str p "("; _pp_ast_typ p in_ty; _print_str p ")");
    _print_str p " -> ";
    if _is_atomic_typ out_ty
    then _pp_ast_typ p out_ty
    else (_print_str p "("; _pp_ast_typ p out_ty; _print_str p ")");
;;

let _binop_to_str (binop : Ast.binary_op) : string =
  match binop with
  | Binop_add  -> "+"
  | Binop_sub  -> "-"
  | Binop_mul  -> "*"
  | Binop_and  -> "&&"
  | Binop_or   -> "||"
  | Binop_eq   -> "="
  | Binop_less -> "<"
;;

let _is_atomic_expr (expr : Ast.expression) : bool =
  match expr.expr_desc with
  | Exp_const _ | Exp_ident _ -> true
  | _ -> false
;;

let rec _pp_ast_expr (p : printer) (expr : Ast.expression) : unit =
  match expr.expr_desc with
  | Exp_const const -> _pp_ast_const p const
  | Exp_ident name -> _print_str p name
  | Exp_binop (binop, lhs, rhs) ->
    if _is_atomic_expr lhs
    then _pp_ast_expr p lhs
    else (_print_str p "("; _pp_ast_expr p lhs; _print_str p ")");
    _print_strs p [" "; (_binop_to_str binop); " ";];
    if _is_atomic_expr rhs
    then _pp_ast_expr p rhs
    else (_print_str p "("; _pp_ast_expr p rhs; _print_str p ")");
  | Exp_let (rec_flag, bindings, body) ->
    _pp_ast_let_bindings p rec_flag bindings;
    _println_str p " in";
    _pp_ast_expr p body;
  | Exp_fun (args, body) ->
    _print_str p "(fun";
    List.iter (fun v -> _print_str p " "; _pp_ast_opt_typ_var p v;) args;
    _println_str p " -> ";
    _inc_space p 2; _pp_ast_expr p body; _dec_space p 2;
    _print_newline p;
    _println_str p ")";
  | Exp_apply (func, args) ->
    _print_str p "(";
    _pp_ast_expr p func;
    List.iter (fun arg -> _print_str p " "; _pp_ast_expr p arg;) args;
    _print_str p ")";
  | Exp_if (cnd, thn, els) ->
    _print_str p "if ("; _pp_ast_expr p cnd; _println_str p ")";
    _print_str p "then "; _inc_space p 5; _pp_ast_expr p thn; _dec_space p 5;
    _print_newline p;
    _print_str p "else "; _inc_space p 5; _pp_ast_expr p els; _dec_space p 5;

and _pp_ast_const (p : printer) (const : Ast.constant) : unit =
  match const with
  | Const_Int n ->  _print_str p (Int.to_string n)
  | Const_Bool b -> _print_str p (if b then "true" else "false")

and _pp_ast_let_bindings
    (p : printer) (rec_flag : Ast.rec_flag) (bindings : Ast.binding list)
  : unit = 
  _print_str p "let ";
  if rec_flag = Ast.Recursive then _print_str p "rec ";
  match bindings with
  | [] -> failwith "[Frontend_pp] Internal Error: empty let bindings";
  | bd::bds ->
    _pp_ast_binding p bd;
    List.iter (fun bd ->
        _print_newline p;
        _print_str p "and ";
        _pp_ast_binding p bd) bds;

and _pp_ast_binding (p : printer) (binding : Ast.binding) : unit =
  _pp_ast_opt_typ_var p binding.binding_lhs;
  _print_str p " = ";
  _pp_ast_expr p binding.binding_rhs;

and _pp_ast_opt_typ_var (p : printer) (var : Ast.opt_typed_var) : unit =
  match var.typ with
  | None -> _print_str p var.var.stuff;
  | Some typ ->
    begin
      _print_str p "(";
      _print_str p var.var.stuff;
      _print_str p " : ";
      _pp_ast_typ p typ;
      _print_str p ")";
    end
;;

let _pp_ast_struct_item (p : printer) (item : Ast.struct_item) : unit =
  match item.struct_item_desc with
  | Struct_eval expr ->
    _pp_ast_expr p expr;
    _print_newline p; _print_newline p;
  | Struct_bind (rec_flag, bindings) -> 
    _pp_ast_let_bindings p rec_flag bindings;
    _println_str p ";;";
    _print_newline p;
;;

let pp_ast_structure (structure : Ast.structure) =
  let p = _create_printer () in
  List.iter (_pp_ast_struct_item p) structure;
  p.buffer
;;
