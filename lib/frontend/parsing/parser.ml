open Pervasives

(* Internal wrapper around lexer to support peeking.
 * [peek ()] returns the same token until [skip ()]. *)
type _tok_stream =
  { skip  : unit -> unit         (* skips peeked token *)
  ; peek  : unit -> Token.t option 
  ; where : unit -> Location.t   (* same as [token_stream.where] *)
  }

let _create_tok_stream (lexer : Lexer.t) : _tok_stream =
  let peeked = ref None in (* None already stands for EOF *)
  let skip () =
    peeked := None
  in
  let peek () =
    match !peeked with
    | None ->
      let opt_tok = Lexer.next lexer in
      peeked := Some opt_tok; opt_tok
    | Some opt_tok -> opt_tok
  in
  let where () =
    Lexer.next_loc lexer
  in
  { skip; peek; where }
;;
(* Ideally _tok_stream should go in a module, but I want to make self-compilation
 * easier; I'll refactor later when soc supports nested module *)


(* All errors will be wrapped into this and [parse] will catch it at the top
 * level, this greatly simplifies internal implementation *)
exception Parser_error of Errors.parser_error
let _error (err : Errors.parser_error) : 'a =
  raise (Parser_error err)
;;

let _error_invalid_int (s : string) (where : Span.t) : 'a =
  _error (Errors.Parser_invalid_integer (s, where))
;;

let _error_on_eof (where : Location.t) (expect : Token.desc list) : 'a =
  _error (Errors.Parser_unexpected_eof (where, expect))
;;

let _error_unexpected_token (actual : Token.t) (expect : Token.desc list) : 'a =
  _error (Errors.Parser_unexpected_token (actual, expect))
;;

(* Only compare token based on category, not internal fields *)
let _get_next_token_expect (s : _tok_stream) (expect : Token.desc) : Token.t =
  match s.peek () with
  | None -> _error_on_eof (s.where ()) [expect]
  | Some actual ->
    s.skip ();
    if expect = actual.token_desc then actual
    else _error_unexpected_token actual [expect]
;;

(* Error on EOF. NOTE only use if a token is absolutely expected  *)
let _peek_token_exn (s : _tok_stream) (expected : Token.desc list) : Token.t =
  match s.peek () with
  | None -> _error_on_eof (s.where ()) expected
  | Some tok -> tok
;;

(* Only compare token based on category, not internal fields *)
let _skip_next_token_expect (s : _tok_stream) (expect : Token.desc) : unit =
  let _ = _get_next_token_expect s expect in ()
;;

let rec _skip_zero_or_more (s : _tok_stream) (to_skip : Token.desc) : unit = 
  match s.peek () with
  | Some { token_desc; _ } when token_desc = to_skip ->
    s.skip(); _skip_zero_or_more s to_skip
  | _ -> ()
;;


(* NOTE
   1. Should we pass along the peeked token?
      - YES: It saves checks for EOF.
      - NO:  It complicates function signature, sometimes disperses calls to
                get/peek token at multiple recursion call sites. 
      So I went with NO. It felt like just moving calls to get/peek token from
      recursion call sites to beginning of called functions. But 1 less arg:).
      With the extra initial check, the impls also read more like grammar rules.

   2. Each function corresponds to a rule; when applicable, the precedence goes
   from low to high (assume reading from top to bottom). After each call, all
   tokens associated with that generation will be consumed.

   3. For any terminal (token) in a rule, the corresponding function MUST
   consume that token (by [_get_next_token_expect] or [_skip_next_token]).
*)

let rec _parse_typ (s : _tok_stream) : Ast.typ =
  _parse_arrow_typ s (* nice forward compatibility :) *)

and _parse_arrow_typ (s : _tok_stream) : Ast.typ = (* right associative *)
  let in_typ = _parse_primary_typ s in
  match s.peek () with 
  | Some tok when tok.token_desc = Rarrow -> s.skip ();
    let out_typ = _parse_arrow_typ s in
    { Ast.typ_desc = Typ_arrow (in_typ, out_typ);
      typ_span = Span.merge in_typ.typ_span out_typ.typ_span }
  | _ -> in_typ (* no arrow *)

and _parse_primary_typ (s : _tok_stream) : Ast.typ = (* int, 'a, (a -> b) *)
                        (* NOTE stay synched! *)
  let expected = Token.[QuoteIdent ""; DecapIdent ""; Lparen] in 
  let tok = _peek_token_exn s expected in
  s.skip ();
  match tok.token_desc with
  | QuoteIdent name ->
    { Ast.typ_desc = Typ_var name; typ_span = tok.token_span }
  | DecapIdent name ->
    { Ast.typ_desc = Typ_const name; typ_span = tok.token_span }
  | Lparen ->
    let typ = _parse_typ s in
    let last = _get_next_token_expect s Rparen in
    { Ast.typ_desc = typ.typ_desc;
      typ_span = (Span.merge tok.token_span last.token_span) }
  | _ -> _error_unexpected_token tok expected
;;


let rec _parse_opt_typed_var (s : _tok_stream) : Ast.opt_typed_var =
  let expected = Token.[DecapIdent ""; Lparen] in (* NOTE stay synched! *)
  let tok = _peek_token_exn s expected in
  s.skip ();
  match tok.token_desc with
  | Lparen ->
    let inner = _parse_opt_typed_var s in _skip_next_token_expect s Rparen;
    inner
  | DecapIdent name -> 
    begin
      let var = { Ast.stuff = name; span = tok.token_span } in
      match s.peek () with
      | Some { token_desc = Colon; _ } -> s.skip ();
        { Ast.var ; typ = Some (_parse_typ s) }
      | _ -> { Ast.var ; typ = None }
    end
  | _ -> _error_unexpected_token tok expected
;;


let rec _parse_expr (s : _tok_stream) : Ast.expression =
  let tok = _peek_token_exn s [Let; If; Fun] in
  match tok.token_desc with
  | Let -> _parse_let_expr s
  | If -> _parse_if_expr s
  | Fun -> _parse_fun_expr s
  | _ -> _parse_logical_or_expr s

and _parse_fun_expr (s : _tok_stream) : Ast.expression =
  let rec _parse_opt_typed_vars (rev_vars : Ast.opt_typed_var list) =
    match (_peek_token_exn s []).token_desc with
    | Rarrow -> List.rev rev_vars
    | _ ->
      let var = _parse_opt_typed_var s in
      _parse_opt_typed_vars (var::rev_vars)
  in
  let fun_tok = _get_next_token_expect s Fun in
  let vars = _parse_opt_typed_vars [] in _skip_next_token_expect s Rarrow;
  let body_expr = _parse_expr s in
  { Ast.expr_desc = Exp_fun (vars, body_expr);
    expr_span = (Span.merge fun_tok.token_span body_expr.expr_span) }

and _parse_let_expr (s : _tok_stream) : Ast.expression =
  let let_tok = _peek_token_exn s [Let] in (* if it's not Let, it'll error later *)
  let bindings, rec_flag, _ = _parse_let_bindings s in
  _parse_let_cont_on_body s let_tok bindings rec_flag

and _parse_let_cont_on_body (s : _tok_stream) (* starting from [In] token *)
    (let_tok : Token.t) (bindings : Ast.binding list) (rec_flag : Ast.rec_flag)
  : Ast.expression =
  _skip_next_token_expect s In;
  let body_expr = _parse_expr s in
  { Ast.expr_desc = Exp_let (rec_flag, bindings, body_expr);
    expr_span = (Span.merge let_tok.token_span body_expr.expr_span) }

and _parse_let_bindings (* returns the span of rhs in last binding (at least 1) *)
    (s : _tok_stream) : (Ast.binding list * Ast.rec_flag * Span.t) =
  let parse_one_binding () =
    let binding_lhs = _parse_opt_typed_var s in _skip_next_token_expect s Equal;
    let binding_rhs = _parse_expr s in
    { Ast.binding_lhs; binding_rhs }
  in
  let parse_rec_flag () : Ast.rec_flag =
    match (_peek_token_exn s []).token_desc with
    | Rec -> s.skip (); Ast.Recursive
    | _ -> Ast.Nonrecursive
  in
  let rec parse_bindings rev_bindings =  (* assume there's 1 more binding *)
    let last_binding = parse_one_binding () in
    let rev_bindings = last_binding::rev_bindings in
    match s.peek () with
    | Some { token_desc = And; _ } -> s.skip (); parse_bindings rev_bindings
    | _ -> (List.rev rev_bindings, last_binding.binding_rhs.expr_span)
  in
  _skip_next_token_expect s Let;
  let rec_flag = parse_rec_flag () in
  let bindings, last_rhs_span = parse_bindings [] in
  (bindings, rec_flag, last_rhs_span)

and _parse_if_expr (s : _tok_stream) : Ast.expression =
  let if_tok = _get_next_token_expect s If in
  let cond_expr = _parse_expr s in _skip_next_token_expect s Then;
  let then_expr = _parse_expr s in _skip_next_token_expect s Else;
  let else_expr = _parse_expr s in
  { Ast.expr_desc = Exp_if (cond_expr, then_expr, else_expr);
    expr_span = (Span.merge if_tok.token_span else_expr.expr_span) }

and _parse_logical_or_expr (s : _tok_stream) : Ast.expression =
  (* right-assoc to speed up short-circuit *)
  _parse_binary_expr_right_assoc s _parse_logical_and_expr
    [ (Token.BarBar, Ast.Binop_or) ] 

and _parse_logical_and_expr (s : _tok_stream) : Ast.expression =
  (* right-assoc to speed up short-circuit *)
  _parse_binary_expr_right_assoc s _parse_relational_expr
    [ (Token.AmperAmper, Ast.Binop_and) ] 

and _parse_binary_expr_right_assoc
    (s : _tok_stream)
    (parse_subexpr : _tok_stream -> Ast.expression)
    (ops : (Token.desc * Ast.binary_op) list)
  : Ast.expression =
  let lhs_expr = parse_subexpr s in
  match s.peek () with 
  | None -> lhs_expr
  | Some tok ->
    match List.assoc_opt tok.token_desc ops with
    | None -> lhs_expr
    | Some binop -> s.skip ();
      (* this recursive call makes it right-assoc *)
      let rhs_expr = _parse_binary_expr_right_assoc s parse_subexpr ops in
      { Ast.expr_desc = Exp_binop (binop, lhs_expr, rhs_expr);
        expr_span = (Span.merge lhs_expr.expr_span rhs_expr.expr_span) }

and _parse_relational_expr (s : _tok_stream) : Ast.expression =
  _parse_binary_expr_left_assoc s _parse_add_sub_expr
    [ (Token.Equal, Ast.Binop_eq)
    ; (Token.Less, Ast.Binop_less) ]

and _parse_add_sub_expr (s : _tok_stream) : Ast.expression =
  _parse_binary_expr_left_assoc s  _parse_mul_expr
    [ (Token.Plus, Ast.Binop_add)
    ; (Token.Minus, Ast.Binop_sub) ]

and _parse_mul_expr (s : _tok_stream) : Ast.expression =
  _parse_binary_expr_left_assoc s _parse_apply_expr
    [ (Token.Asterisk, Ast.Binop_mul) ]

and _parse_binary_expr_left_assoc
    (s : _tok_stream)
    (parse_subexpr : _tok_stream -> Ast.expression)
    (ops : (Token.desc * Ast.binary_op) list)
  : Ast.expression =
  let rec go lhs_expr =
    match s.peek () with 
    | None -> lhs_expr
    | Some tok ->
      match List.assoc_opt tok.token_desc ops with
      | None -> lhs_expr
      | Some binop -> s.skip ();
        let rhs_expr = parse_subexpr s in
        let lhs_expr = (* left-assoc *)
          { Ast.expr_desc = Exp_binop (binop, lhs_expr, rhs_expr);
            expr_span = (Span.merge lhs_expr.expr_span rhs_expr.expr_span) } in
        go lhs_expr
  in
  go (parse_subexpr s)

and _parse_apply_expr (* > 1 consecutive primary expr *)
    (s : _tok_stream) : Ast.expression =
  let func_expr = _parse_primary_expr s in
  let finalize (rev_args : Ast.expression list) =
    match rev_args with
    | [] -> func_expr (* no args, not an application *)
    | last_arg::_ ->
      let arg_exprs = List.rev rev_args in
      { Ast.expr_desc = Exp_apply (func_expr, arg_exprs);
        expr_span = (Span.merge func_expr.expr_span last_arg.expr_span) }
  in
  let rec collect_args (rev_args : Ast.expression list) =
    match s.peek () with
    | Some ( (* NOTE this case must agree with _parse_primary_expr *)
        { token_desc = (True | False | Int _ | DecapIdent _ | Lparen); _}) ->
      let arg = _parse_primary_expr s in
      collect_args (arg::rev_args)
    | _ -> finalize rev_args
  in
  collect_args []

and _parse_primary_expr (* e.g., constant, var, parenthesized expr *)
    (s : _tok_stream) : Ast.expression =
  let tok = _peek_token_exn s [] in
  (* NOTE when modifying this, check functions that rely on the
   * FIRST set of primary_expr *)
  match tok.token_desc with
  | Lparen -> 
    s.skip ();
    let expr = _parse_expr s in
    let last = _get_next_token_expect s Rparen in
    { Ast.expr_desc = expr.expr_desc
    ; expr_span = (Span.merge tok.token_span last.token_span) }
  | _ -> _parse_constant_or_var s

and _parse_constant_or_var
    (s : _tok_stream) : Ast.expression =
  let parse_int_expr (str : string) (where : Span.t) : Ast.expression =
    match Int.of_string_opt str with
    | None -> _error_invalid_int str where
    | Some n -> { Ast.expr_desc = Exp_const (Const_Int n); expr_span = where }
  in
  (* NOTE stay synched! *)
  let expected = Token.[True; False; Int ""; DecapIdent "";] in 
  let tok = _peek_token_exn s expected in
  s.skip ();
  (* NOTE when modifying this, check functions that rely on the
   * FIRST set of this rule *)
  match tok.token_desc with
  | True -> { Ast.expr_desc = Exp_const (Const_Bool true)
            ; expr_span = tok.token_span }
  | False -> { Ast.expr_desc = Exp_const (Const_Bool false)
             ; expr_span = tok.token_span }
  | Int int_str -> parse_int_expr int_str tok.token_span
  | DecapIdent str ->
    { Ast.expr_desc = Ast.Exp_ident str; expr_span = tok.token_span }
  | _ -> _error_unexpected_token tok expected
;;


(* Either a top-level let-expression or binding (let without body) *)
let _parse_let_or_bind (s : _tok_stream) : Ast.struct_item =
  let let_tok = _peek_token_exn s [Let] in
  let bindings, rec_flag, last_rhs_span = _parse_let_bindings s in
  match s.peek () with
  | Some { token_desc = In; _ } ->
    let let_expr = _parse_let_cont_on_body s let_tok bindings rec_flag in
    { Ast.struct_item_desc = Struct_eval let_expr
    ; struct_item_span = let_expr.expr_span }
  | Some { token_desc = SemiSemiColon; token_span } ->
    s.skip ();
    { Ast.struct_item_desc = Struct_bind (rec_flag, bindings);
      struct_item_span = Span.merge let_tok.token_span token_span }
  | _ ->
    { Ast.struct_item_desc = Struct_bind (rec_flag, bindings);
      struct_item_span = Span.merge let_tok.token_span last_rhs_span }
;;

let _parse_struct_item (s : _tok_stream) : Ast.struct_item =
  match (_peek_token_exn s []).token_desc with
  | Let ->
    _parse_let_or_bind s 
  | _ -> 
    let expr = _parse_expr s in
    { Ast.struct_item_desc = Struct_eval expr;
      struct_item_span = expr.expr_span }
;;

let rec _parse_structure (s : _tok_stream) : Ast.structure =
  _skip_zero_or_more s SemiSemiColon;
  match s.peek () with
  | None -> []
  | Some _ ->
    let item = _parse_struct_item s in
    let rest = _parse_structure s in
    item::rest
;;

let parse (lexer : Lexer.t) =
  try 
    let internal_tok_stream = _create_tok_stream lexer in
    let structure = _parse_structure internal_tok_stream in
    Ok structure
  with Parser_error(err) -> Error err
;;
