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
    String.join_with [ "expecting '"; (Char.to_string expect); "'" ] ""
  | Lexing_number -> "lexing a number"
  | Lexing_identifier_or_keyword -> "lexing an identifier or keyword"
;;

let pp_lexer_error (err : Errors.lexer_error) : string =
  match err with
  | Lexer_unexpected_char (expect, actual, loc) ->
    String.join_with
      [ "[Lexer] Expected '"; (Char.to_string expect);
        "', but got '"; (Char.to_string actual); "'";
        " at "; (Location.to_string loc); ]
      ""
  | Lexer_unexpected_eof (where, action) ->
    String.join_with
      [ "[Lexer] Unexpected EOF while "; (_pp_lexer_action action);
        " at "; (Location.to_string where); ]
      ""
  | Lexer_invalid_start (start, loc) ->
    String.join_with
      [ "[Lexer] Invalid start of token: '"; (Char.to_string start);
        "' at "; (Location.to_string loc); ]
      ""
;;


let _pp_span (span : Span.t) : string =
  String.join_with
    [ "<"; (Location.to_string span.start); "-";
      (Location.to_string span.final); ">" ]
    ""
;;

(* Whether to show content of [Token.Int], etc. *)
type token_desc_visibility =
  | Show_content
  | Hide_content

let _pp_token_desc_impl desc (visibility : token_desc_visibility) =
  match desc, visibility with
  | Token.Plus, _ -> "<Plus>"
  | Token.Minus, _ -> "<Minus>"
  | Token.Asterisk, _ -> "<Asterisk>"
  | Token.AmperAmper, _ -> "<AmperAmper>"
  | Token.BarBar, _ -> "<BarBar>"
  | Token.If, _ -> "<If>"
  | Token.Then, _ -> "<Then>"
  | Token.Else, _ -> "<Else>"
  | Token.Let, _ -> "<Let>"
  | Token.Rec, _ -> "<Rec>"
  | Token.Colon, _ -> "<Colon>"
  | Token.Equal, _ -> "<Equal>"
  | Token.And, _ -> "<And>"
  | Token.In, _ -> "<In>"
  | Token.Lparen, _ -> "<Lparen>"
  | Token.Rparen, _ -> "<Rparen>"
  | Token.Rarrow, _ -> "<Rarrow>"
  | Token.Fun, _ -> "<Fun>"
  | Token.Less, _ -> "<Less>"
  | Token.True, _ -> "<True>"
  | Token.False, _ -> "<False>"
  | Token.Int _, Hide_content -> "<Int>"
  | Token.Int s, Show_content -> String.join_with ["<Int ("; s; ")>"] ""
  | Token.DecapIdent _, Hide_content -> "<DecapIdent>"
  | Token.DecapIdent s, Show_content ->
    String.join_with ["<DecapIdent ("; s; ")>"] ""
  | Token.QuoteIdent _, Hide_content -> "<QuoteIdent>"
  | Token.QuoteIdent s, Show_content ->
    String.join_with ["<QuoteIdent ("; s; ")>"] ""
  | Token.SemiSemiColon, _ -> "<SemiSemiColon>"
;;

let pp_token_desc desc =
  _pp_token_desc_impl desc Show_content
;;

let pp_token (tok : Token.t) =
  String.join_with
    [ "{"; (pp_token_desc tok.token_desc);
      " in <"; tok.token_span.filename; ":"; (_pp_span tok.token_span); ">}"; ]
    ""
;;

let pp_parser_error (err : Errors.parser_error) =
  let pp_expected_tokens (toks : Token.desc list) : string =
    match toks with
    | [] -> "<unknown>"
    | _ ->
      let tok_strs =
        List.map (fun tok -> _pp_token_desc_impl tok Hide_content) toks in
      let inner = String.join_with tok_strs "; " in
      String.join_with ["["; inner; "]"] ""
  in
  match err with
  | Parser_invalid_integer (text, span) ->
    String.join_with
      [ "[Parser]: Invalid integer token <"; text; "> at "; (_pp_span span); ]
      ""
  | Parser_unexpected_token (actual, expects) ->
    String.join_with
      [ "[Parser]: Unexpected token "; (pp_token actual);
        " where expected tokens are "; (pp_expected_tokens expects); ]
      ""
  | Parser_unexpected_eof (eof_loc, expects) ->
    String.join_with
      [ "[Parser]: Unexpected EOF at "; (Location.to_string eof_loc);
        " where expected tokens are "; (pp_expected_tokens expects); ]
      ""
;;

let pp_ast_interp_error (err : Errors.ast_interp_error) =
  match err with
  | Ast_interp_unbound_var (name, span) ->
    String.join_with
      [ "[Ast_interp]: Unbound variable <"; name; "> at "; (_pp_span span); ]
      ""
  | Ast_interp_type_mismatch (expect, actual, span) ->
    String.join_with
      [ "[Ast_interp]: Expected type <"; expect; ">";
        " but got <"; actual; "> at "; (_pp_span span); ]
      ""
  | Ast_interp_arity_mismatch (expect, actual, span) ->
    String.join_with
      [ "[Ast_interp]: Expected arity "; (Int.to_string expect);
        " but got "; (Int.to_string actual); " at "; (_pp_span span); ]
      ""
  | Ast_interp_letrec_invalid_rhs span ->
    String.append "Invalid rhs of let rec binding at " (_pp_span span)
;;


let _is_atomic_typ (typ : Ast.typ) : bool =
  match typ.typ_desc with
  | Typ_const _ | Typ_var _ -> true
  | _ -> false
;;

let rec _pp_ast_typ (p : printer) (typ : Ast.typ) : unit =
  match typ.typ_desc with
  | Typ_const name -> _print_str p name;
  | Typ_var name -> _print_str p "'"; _print_str p name;
  | Typ_arrow (in_ty, out_ty) ->
    _pp_ast_typ_parens_on_non_atomic p in_ty;
    _print_str p " -> ";
    _pp_ast_typ_parens_on_non_atomic p out_ty;

and _pp_ast_typ_parens_on_non_atomic (p : printer) (typ : Ast.typ)
  : unit =
  if _is_atomic_typ typ
  then _pp_ast_typ p typ
  else (_print_str p "("; _pp_ast_typ p typ; _print_str p ")");
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
    _pp_ast_expr_parens_on_non_atomic p lhs;
    _print_strs p [" "; (_binop_to_str binop); " ";];
    _pp_ast_expr_parens_on_non_atomic p rhs
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
    _pp_ast_expr p func;
    List.iter (fun arg ->
        _print_str p " "; (* first space separates func and arg *)
        _pp_ast_expr_parens_on_non_atomic p arg;)
      args;
  | Exp_if (cnd, thn, els) ->
    _print_str p "if "; _pp_ast_expr_parens_on_non_atomic p cnd;
    _print_newline p;
    _print_str p "then "; _inc_space p 5; _pp_ast_expr p thn; _dec_space p 5;
    _print_newline p;
    _print_str p "else "; _inc_space p 5; _pp_ast_expr p els; _dec_space p 5;

and _pp_ast_expr_parens_on_non_atomic (p : printer) (expr : Ast.expression)
  : unit =
  if _is_atomic_expr expr
  then _pp_ast_expr p expr
  else (_print_str p "("; _pp_ast_expr p expr; _print_str p ")");

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
        _pp_ast_binding p bd)
      bds;

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
