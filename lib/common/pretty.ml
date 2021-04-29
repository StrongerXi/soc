open Pervasives

(* Again, this deserves a module but to simplify self-compilation... TODO *)
type printer = 
  { mutable space_level : int  (* # of spaces *)
  ; mutable buffer      : string
  ; mutable is_newline  : bool (* are we at beginning of a new line? *)
  ; print_typ_annot     : bool (* print type annotations? *)
  }


let _create_printer () print_typ_annot =
  { space_level = 0; buffer = ""; is_newline = true; print_typ_annot
  }
;;

let _inc_space (p : printer) (n : int) : unit =
  p.space_level <- p.space_level + n
;;

let _dec_space (p : printer) (n : int) : unit =
  p.space_level <- p.space_level - n
;;

let __print_str_only (p : printer) (s : string) : unit =
  p.buffer <- p.buffer ^ s
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


let pp_lexer_error (err : Errors.lexer_error) : string =
  match err with
  | Lexer_unexpected_char (expect, actual, loc) ->
    String.concat
      [ "[Lexer] Expected '"; (Char.to_string expect);
        "', but got '"; (Char.to_string actual); "'";
        " at "; (Location.to_string loc); ]

  | Lexer_unexpected_eof (where, expected) ->
    String.concat
      [ "[Lexer] Unexpected EOF while expecting "; (Char.to_string expected);
        " at "; (Location.to_string where); ]

  | Lexer_invalid_start (start, loc) ->
    String.concat
      [ "[Lexer] Invalid start of token: '"; (Char.to_string start);
        "' at "; (Location.to_string loc); ]
;;


let _pp_span (span : Span.t) : string =
  String.concat
    [ "<"; (Location.to_string span.start); "-";
      (Location.to_string span.final); ">" ]
;;

(* Whether to show content of [Token.Int], etc. *)
type token_desc_visibility =
  | Show_content
  | Hide_content

let _pp_token_desc_impl desc (visibility : token_desc_visibility) =
  let pp_tok_with_content (tok : string) (content : string) =
    match visibility with
    | Hide_content -> String.concat ["<"; tok; ">"]
    | Show_content -> String.concat ["<"; tok; " ("; content; ")>"]
  in
  match desc with
  | Token.AmperAmper -> "<AmperAmper>"
  | Token.BarBar -> "<BarBar>"
  | Token.If -> "<If>"
  | Token.Then -> "<Then>"
  | Token.Else -> "<Else>"
  | Token.Let -> "<Let>"
  | Token.Rec -> "<Rec>"
  | Token.Colon -> "<Colon>"
  | Token.Underscore -> "<Underscore>"
  | Token.Equal -> "<Equal>"
  | Token.And -> "<And>"
  | Token.In -> "<In>"
  | Token.Lparen -> "<Lparen>"
  | Token.Rparen -> "<Rparen>"
  | Token.Rarrow -> "<Rarrow>"
  | Token.Fun -> "<Fun>"
  | Token.True -> "<True>"
  | Token.False -> "<False>"
  | Token.Int s        -> pp_tok_with_content "Int" s
  | Token.DecapIdent s -> pp_tok_with_content "DecapIdent" s
  | Token.QuoteIdent s -> pp_tok_with_content "QuoteIdent" s
  | Token.InfixOp0 s   -> pp_tok_with_content "InfixOp0" s
  | Token.InfixOp1 s   -> pp_tok_with_content "InfixOp1" s
  | Token.InfixOp2 s   -> pp_tok_with_content "InfixOp2" s
  | Token.InfixOp3 s   -> pp_tok_with_content "InfixOp3" s
  | Token.SemiSemiColon -> "<SemiSemiColon>"
;;

let pp_token_desc desc =
  _pp_token_desc_impl desc Show_content
;;

let pp_token (tok : Token.t) =
  String.concat
    [ "{"; (pp_token_desc tok.token_desc);
      " at "; (_pp_span tok.token_span); "}"; ]
;;

let pp_parser_error (err : Errors.parser_error) =
  let pp_expected_tokens (toks : Token.desc list) : string =
    match toks with
    | [] -> "<unknown>"
    | _ -> List.to_string toks (fun tok -> _pp_token_desc_impl tok Hide_content)
  in
  match err with
  | Parser_invalid_integer (text, span) ->
    String.concat
      [ "[Parser]: Invalid integer token <"; text; "> at "; (_pp_span span); ]

  | Parser_unexpected_token (actual, expects) ->
    String.concat
      [ "[Parser]: Unexpected token "; (pp_token actual);
        " where expected tokens are "; (pp_expected_tokens expects); ]

  | Parser_unexpected_eof (eof_loc, expects) ->
    String.concat
      [ "[Parser]: Unexpected EOF at "; (Location.to_string eof_loc);
        " where expected tokens are "; (pp_expected_tokens expects); ]
;;

let pp_ast_interp_error (err : Errors.ast_interp_error) =
  match err with
  | Ast_interp_unbound_var (name, span) ->
    String.concat
      [ "[Ast_interp]: Unbound variable <"; name; "> at "; (_pp_span span); ]

  | Ast_interp_type_mismatch (expect, actual, span) ->
    String.concat
      [ "[Ast_interp]: Expected type <"; expect; ">";
        " but got <"; actual; "> at "; (_pp_span span); ]

  | Ast_interp_arity_mismatch (expect, actual, span) ->
    String.concat
      [ "[Ast_interp]: Expected arity "; (Int.to_string expect);
        " but got "; (Int.to_string actual); " at "; (_pp_span span); ]

  | Ast_interp_letrec_invalid_rhs span ->
    "Invalid rhs of let rec binding at " ^ (_pp_span span)
;;


let _is_atomic_typ_desc (desc : Ast.typ) : bool =
  match desc with
  | Typ_const _ | Typ_var _ -> true
  | _ -> false
;;

let rec _pp_ast_typ_desc (p : printer) (desc : Ast.typ) : unit =
  match desc with
  | Typ_const name -> _print_str p name;
  | Typ_var None -> _print_str p "_";
  | Typ_var (Some name) -> _print_str p "'"; _print_str p name;
  | Typ_arrow (in_ty, out_ty) ->
    _pp_ast_typ_parens_on_non_atomic p in_ty;
    _print_str p " -> ";
    match out_ty with (* arrow is right associative *)
    | Typ_arrow _ -> _pp_ast_typ_desc p out_ty;
    | _ -> _pp_ast_typ_parens_on_non_atomic p out_ty;

and _pp_ast_typ_parens_on_non_atomic (p : printer) (typ : Ast.typ)
  : unit =
  if _is_atomic_typ_desc typ
  then _pp_ast_typ_desc p typ
  else (_print_str p "("; _pp_ast_typ_desc p typ; _print_str p ")");
;;

let pp_ast_typ (desc : Ast.typ) =
  let p = _create_printer () false in
  _pp_ast_typ_desc p desc;
  p.buffer
;;

let _is_atomic_expr (expr : Ast.expression) : bool =
  match expr.expr_desc with
  | Exp_const _ | Exp_ident _ -> true
  | _ -> false
;;

let _may_fit_on_oneline (expr : Ast.expression) : bool =
  match expr.expr_desc with
  | Exp_if _ | Exp_let _ -> false
  | Exp_const _ | Exp_ident _ | Exp_fun _ | Exp_apply _ -> true
;;

(* e.g., " : int" *)
let _pp_typ_annot (p : printer) (annot : Ast.typ option) : unit =
  _print_str p " : ";
  match annot with
  | None -> _print_str p "_"
  | Some typ -> _pp_ast_typ_desc p typ
;;

let rec _pp_ast_expr (p : printer) (expr : Ast.expression) : unit =
  if p.print_typ_annot then _print_str p "(";
  _pp_ast_expr_wo_typ_annot p expr;
  if p.print_typ_annot
  then (_pp_typ_annot p expr.expr_typ; _print_str p ")");

and _pp_ast_expr_wo_typ_annot (p : printer) (expr : Ast.expression) : unit =
  match expr.expr_desc with
  | Exp_const const -> _pp_ast_const p const
  | Exp_ident name -> _print_str p name
  | Exp_let (rec_flag, bindings, body) ->
    _pp_ast_let_bindings p rec_flag bindings;
    _println_str p " in";
    _pp_ast_expr p body;
  | Exp_fun (args, body) ->
    _print_str p "(fun";
    List.iter (fun v -> _print_str p " "; _pp_ast_opt_typ_var p v;) args;
    _print_str p " -> ";
    if _may_fit_on_oneline body
    then _pp_ast_expr p body
    else (_print_newline p;
          _inc_space p 2; _pp_ast_expr p body; _dec_space p 2;
          _print_newline p;);
    _print_str p ")";
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
  if _may_fit_on_oneline binding.binding_rhs
  then _pp_ast_expr p binding.binding_rhs
  else
    (_print_newline p;
     _inc_space p 2; _pp_ast_expr p binding.binding_rhs; _dec_space p 2;)

and _pp_ast_opt_typ_var (p : printer) (otv : Ast.opt_typed_var) : unit =
  match otv.typ with
  | None -> _print_str p otv.var;
  | Some typ ->
    begin
      _print_str p "(";
      _print_str p otv.var;
      _print_str p " : ";
      _pp_ast_typ_desc p typ;
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
    _print_newline p;
    _println_str p ";;";
    _print_newline p;
;;

let pp_ast_structure (structure : Ast.structure) =
  let p = _create_printer () false in
  List.iter (_pp_ast_struct_item p) structure;
  p.buffer
;;

let pp_ast_expr (expr : Ast.expression) =
  let p = _create_printer () false in
  _pp_ast_expr p expr;
  p.buffer
;;

let pp_ast_structure_with_typ_annot (structure : Ast.structure) =
  let p = _create_printer () true in
  List.iter (_pp_ast_struct_item p) structure;
  p.buffer
;;

let pp_ast_expr_with_typ_annot (expr : Ast.expression) =
  let p = _create_printer () true in
  _pp_ast_expr p expr;
  p.buffer
;;

let pp_typer_error (err : Errors.typer_error) =
  match err with
  | Typer_unbound_var (name, span) ->
    String.concat
      [ "[Typer]: Unbound variable <"; name; "> at "; (_pp_span span); ]

  | Typer_type_mismatch (expect, actual, span) ->
    String.concat
      [ "[Typer]: Expected type <"; (pp_ast_typ expect); ">";
        " but got <"; (pp_ast_typ actual); "> at "; (_pp_span span); ]

  | Typer_illegal_letrec_rhs span ->
    "[Typer]: Illegal rhs of let rec binding at " ^ (_pp_span span)

  | Typer_tyvar_occurs (expect, actual, actual_span, tv_name, occurree) ->
    String.concat
      [ "[Typer]: Expected type <"; (pp_ast_typ expect); ">";
        " but got <"; (pp_ast_typ actual); "> at "; (_pp_span actual_span);
        ". The type variable '"; tv_name;
        " occurs within "; (pp_ast_typ occurree) ]
;;


let rec _pp_cir_expr (p : printer) (e : Cir.expr) : unit =
  match e with
  | Cconst const -> _pp_cir_const p const
  | Cident name -> _print_str p name
  | Cmk_closure mkcls -> _pp_cir_mk_closure p mkcls
                           
  | Clet (bds, body) ->
    _print_str p "let ";
    _inc_space p 4;
    List.iter
      (fun (name, rhs_e) ->
         _print_strs p [name; " = "];
         let spaces = (String.length name) + (String.length " = ") in
         _inc_space p spaces; _pp_cir_expr p rhs_e; _dec_space p spaces;
         _print_newline p;)
      bds;
    _dec_space p 4;
    _print_str p "in ";
    _inc_space p 3; _pp_cir_expr p body;_dec_space p 3;

  | Cletrec (bds, body) ->
    _print_str p "let rec ";
    _inc_space p 8;
    List.iter
      (fun (name, (rhs_e : Cir.letrec_rhs)) ->
         _print_strs p [name; " = "];
         let spaces = (String.length name) + (String.length " = ") in
         _inc_space p spaces; _pp_cir_letrec_rhs p rhs_e; _dec_space p spaces;
         _print_newline p;)
      bds;
    _dec_space p 8;
    _print_str p "in ";
    _inc_space p 3; _pp_cir_expr p body;_dec_space p 3;

  | Cif (cnd, thn, els) ->
    _print_str p "if "; _pp_cir_expr p cnd;
    _print_newline p;
    _print_str p "then "; _inc_space p 5; _pp_cir_expr p thn; _dec_space p 5;
    _print_newline p;
    _print_str p "else "; _inc_space p 5; _pp_cir_expr p els; _dec_space p 5;

  | Cprimop (op_kind, args) ->
    _print_str p "(";
    _print_str p (Primops.get_opstr op_kind);
    List.iter (fun arg ->
        _print_str p " "; (* first space separates func and arg *)
        _pp_cir_expr p arg;)
      args;
    _print_str p ")";

  | Capply (func, args) ->
    _print_str p "(";
    _pp_cir_expr p func;
    List.iter (fun arg ->
        _print_str p " "; (* first space separates func and arg *)
        _pp_cir_expr p arg;)
      args;
    _print_str p ")";

  | Cnative_apply (name, args) ->
    _print_strs p ["(<native:" ;name; ">"];
    List.iter (fun arg ->
        _print_str p " "; (* first space separates func and arg *)
        _pp_cir_expr p arg;)
      args;
    _print_str p ")";

and _pp_cir_const (p : printer) (const : Cir.constant) : unit =
  match const with
  | CInt n  -> _print_str p (Int.to_string n)
  | CBool b -> _print_str p (Bool.to_string b)

and _pp_cir_mk_closure (p : printer) (mkcls : Cir.mk_closure) : unit =
  _print_strs p ["(MK_CLOSURE <"; mkcls.cls_name; "> "];
  _print_str p (String.join_with mkcls.free_vars " ");
  _print_str p ")";

and _pp_cir_letrec_rhs (p : printer) (rhs : Cir.letrec_rhs) : unit =
  match rhs with
  | Rhs_const const -> _pp_cir_const p const
  | Rhs_mkcls mkcls -> _pp_cir_mk_closure p mkcls

let _pp_cir_funcs (p : printer) (funcs : (string, Cir.closure) Map.t) : unit =
  let _pp_one_func (name : string) (cls : Cir.closure) : unit =
    _print_strs p ["closure <"; name; "> = {"]; _print_newline p;
    _inc_space p 2;
    _print_strs p ["args = ("; String.join_with cls.args ", "; ")"];
    _print_newline p;
    _print_strs p ["free_vars = ("; String.join_with cls.free_vars ", "; ")"];
    _print_newline p;
    _println_str p "body = {";
    _inc_space p 2;
    _pp_cir_expr p cls.body; _print_newline p;
    _dec_space p 2; _println_str p "}";
    _dec_space p 2; _println_str p "}"
  in
  let _ = Map.mapi _pp_one_func funcs in
  ()
;;

let _pp_cir (p : printer) (cir : Cir.prog) : unit =
  _pp_cir_funcs p cir.closures;
  _pp_cir_expr p cir.expr;
;;

let pp_cir cir =
  let p = _create_printer () true in
  _pp_cir p cir;
  p.buffer
;;


let _pp_lir_op (p : printer) (op : Lir.op) : unit =
  match op with
  | Add -> _print_str p "+"
  | Sub -> _print_str p "-"
  | Mul -> _print_str p "*"
  | Div -> _print_str p "/"
;;

let rec _pp_lir_expr (p : printer) (expr : Lir.expr) : unit =
  match expr with
  | Imm n ->
    _print_str p (Int.to_string n);

  | Tmp temp ->
    _print_str p (Temp.to_string temp);

  | Op (op, lhs_e, rhs_e) ->
    _print_str p "(";
    _pp_lir_expr p lhs_e;
    _print_str p ") ";
    _pp_lir_op p op;
    _print_str p " (";
    _pp_lir_expr p rhs_e;
    _print_str p ")"

  | Call (label_temp, arg_es) ->
    _print_str p "CALL ";
    _print_str p (Temp.to_string label_temp);
    _print_str p " WITH (";
    List.iter 
      (fun arg_e ->
         _pp_lir_expr p arg_e;
         _print_str p ", ")
      arg_es;
    _print_str p ")"

  | NativeCall (label, arg_es) ->
    _print_str p "CALL <";
    _print_str p (Label.to_string label);
    _print_str p "> WITH (";
    List.iter 
      (fun arg_e ->
         _pp_lir_expr p arg_e;
         _print_str p ", ")
      arg_es;
    _print_str p ")"

  | Mem_alloc nbytes ->
    _print_strs p ["ALLOC "; Int.to_string nbytes; " BYTES"];

and _pp_lir_cond (p : printer) (cond : Lir.cond) : unit =
  match cond with
  | True -> _print_str p "TRUE"
  | Less (lhs_e, rhs_e) -> 
    _print_str p "(";
    _pp_lir_expr p lhs_e;
    _print_str p ") < (";
    _pp_lir_expr p rhs_e;
    _print_str p ")"
  | Equal (lhs_e, rhs_e) ->
    _print_str p "(";
    _pp_lir_expr p lhs_e;
    _print_str p ") == (";
    _pp_lir_expr p rhs_e;
    _print_str p ")"
;;

let _pp_lir_deref_expr (p : printer) (expr : Lir.expr) : unit =
  _print_str p "*(";
  _pp_lir_expr p expr;
  _print_str p ")";
;;

let _pp_lir_instr (p : printer) (instr : Lir.instr) : unit =
  match instr with
  | Label label ->
    _dec_space p 2;
    _println_str p (Label.to_string label);
    _inc_space p 2;

  | Load (expr, temp) ->
    _print_strs p [Temp.to_string temp; " := "];
    _pp_lir_expr p expr;
    _print_newline p;

  | LoadMem (expr, temp) ->
    _print_strs p [Temp.to_string temp; " := "];
    _pp_lir_deref_expr p expr;
    _print_newline p;

  | Store (src_e, dst_e) ->
    _pp_lir_deref_expr p dst_e;
    _print_str p " := ";
    _pp_lir_expr p src_e;
    _print_newline p;

  | Store_label (label, dst_e) ->
    _pp_lir_deref_expr p dst_e;
    _print_strs p [" := <"; (Label.to_string label); ">"];
    _print_newline p;

  | Jump (cond, label) ->
    _print_strs p ["JUMP TO <"; (Label.to_string label); "> IF "];
    _pp_lir_cond p cond;
    _print_newline p;

  | Ret expr ->
    _print_str p "RET ";
    _pp_lir_expr p expr;
    _print_newline p;
;;

let _pp_lir_instrs (p : printer) (instrs : Lir.instr list) : unit =
  List.iter (_pp_lir_instr p) instrs
;;

let _pp_lir_func (p : printer) (func : Lir.func) : unit =
  _print_strs p ["<"; Label.to_string func.name; ">:"]; _print_newline p;
  let arg_strs = List.map Temp.to_string func.ordered_args in
  _print_strs p ["args: ["; String.join_with arg_strs ", "; "]"]; _print_newline p;
  _println_str p "body:";
  _inc_space p 2;
  _pp_lir_instrs p func.body;
  _dec_space p 2;
;;

let _pp_lir_prog (p : printer) (prog : Lir.prog) : unit =
  List.iter 
    (fun func ->
       _pp_lir_func p func;
       _print_newline p)
    prog.funcs;
  _println_str p "|<MAIN_ENTRY>|:";
  _inc_space p 2;
  _pp_lir_instrs p prog.entry;
  _dec_space p 2;
;;

let pp_lir_prog prog =
  let p = _create_printer () true in
  _pp_lir_prog p prog;
  p.buffer
;;


let pp_vasm_liveness_annot vasm_annot_pairs =
  List.to_string vasm_annot_pairs
    (fun (vasm, (annot : Liveness_analysis.annot)) -> 
       let vasm_str = Vasm.pp vasm in
       let annot_str = Set.to_string Temp.to_string annot.live_out in
       String.join_with [vasm_str; annot_str] " # ")
;;
