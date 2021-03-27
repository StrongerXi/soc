open Pervasives

type token_stream =
  { next  : unit -> Token.t option
  ; where : unit -> Location.t
  }

(* Internal wrapper around token_stream to support peeking.
 * [peek ()] returns the same token until [next ()]. *)
type _tok_stream =
  { next  : unit -> Token.t option (* NOTE skips peeked token *)
  ; peek  : unit -> Token.t option 
  ; where : unit -> Location.t   (* same as [token_stream.where] *)
  }

let _create_tok_stream (s : token_stream) : _tok_stream =
  let peeked = ref None in (* None already stands for EOF *)
  let next () =
    match !peeked with
    | None -> s.next ()
    | Some opt_tok -> peeked := None; opt_tok (* clears [peeked] *)
  in
  let peek () =
    match !peeked with
    | None ->
      let opt_tok = s.next () in
      peeked := Some opt_tok; opt_tok
    | Some opt_tok -> opt_tok
  in
  { next; peek; where = s.where }
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
  match s.next () with
  | None -> _error_on_eof (s.where ()) [expect]
  | Some actual ->
    if expect = actual.token_desc then actual
    else _error_unexpected_token actual [expect]
;;

(* If EOF is encountered, error with location info *)
let _peek_token_exn (s : _tok_stream) (expected : Token.desc list) : Token.t =
  match s.peek () with
  | None -> _error_on_eof (s.where ()) expected
  | Some tok -> tok
;;

(* Only compare token based on category, not internal fields *)
let _skip_next_token_expect (s : _tok_stream) (expect : Token.desc) : unit =
  let _ = _get_next_token_expect s expect in ()
;;

(* ignores EOF *)
let _skip_next_token (s : _tok_stream) : unit =
  let _ = s.next () in ()
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

and _parse_arrow_typ (s : _tok_stream) : Ast.typ =
  let in_typ = _parse_primary_typ s in
  match s.peek () with 
  | Some tok when tok.token_desc = Rarrow -> 
    _skip_next_token s;
    let out_typ = _parse_primary_typ s in
    { Ast.typ_desc = Typ_arrow (in_typ, out_typ);
      typ_span = Span.merge in_typ.typ_span out_typ.typ_span }
  | _ -> in_typ (* no arrow *)
  
and _parse_primary_typ (s : _tok_stream) : Ast.typ = (* int, (a -> b), etc. *)
  let expected = Token.[DecapIdent ""; Lparen] in (* NOTE stay synched! *)
  let tok = _peek_token_exn s expected in
  _skip_next_token s;
  match tok.token_desc with
  | DecapIdent name ->
    { Ast.typ_desc = Typ_const name; typ_span = tok.token_span }
  | Lparen ->
    let typ = _parse_typ s in
    let last = _get_next_token_expect s Rparen in
    { Ast.typ_desc = typ.typ_desc;
      typ_span = (Span.merge tok.token_span last.token_span) }
  | _ -> _error_unexpected_token tok expected

and _parse_opt_typed_var (s : _tok_stream) : Ast.opt_typed_var =
  let expected = Token.[DecapIdent ""; Lparen] in (* NOTE stay synched! *)
  let tok = _peek_token_exn s expected in
  _skip_next_token s;
  match tok.token_desc with
  | Lparen ->
    let inner = _parse_opt_typed_var s in
    _skip_next_token_expect s Rparen;
    inner
  | DecapIdent name -> 
    begin
      let var = { Ast.stuff = name; span = tok.token_span } in
      match s.peek () with
      | Some { token_desc = Colon; _ } ->
        _skip_next_token s;
        let typ = _parse_typ s in
        { Ast.var ; typ = Some typ }
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
      let acc = (_parse_opt_typed_var s)::rev_vars in
      _parse_opt_typed_vars acc
  in
  let fun_tok = _get_next_token_expect s Fun in
  let vars = _parse_opt_typed_vars [] in
  _skip_next_token_expect s Rarrow;
  let body_expr = _parse_expr s in
  { Ast.expr_desc = Exp_fun (vars, body_expr);
    expr_span = (Span.merge fun_tok.token_span body_expr.expr_span) }

and _parse_let_expr (s : _tok_stream) : Ast.expression =
  let let_tok = _peek_token_exn s [Let] in (* if it's not Let, it'll error later *)
  let bindings, rec_flag = _parse_let_bindings s in
  _skip_next_token_expect s In;
  let body_expr = _parse_expr s in
  { Ast.expr_desc = Exp_let (rec_flag, bindings, body_expr);
    expr_span = (Span.merge let_tok.token_span body_expr.expr_span) }

and _parse_let_bindings (s : _tok_stream) : (Ast.binding list * Ast.rec_flag) =
  let parse_binding () =
    let binding_lhs = _parse_opt_typed_var s in
    _skip_next_token_expect s Equal;
    let binding_rhs = _parse_expr s in
    { Ast.binding_lhs; binding_rhs }
  in
  let parse_rec_flag () : Ast.rec_flag =
    match (_peek_token_exn s []).token_desc with
    | Rec -> _skip_next_token s; Ast.Recursive
    | _ -> Ast.Nonrecursive
  in
  let rec go rev_bindings =  (* at least 1 binding *)
    let rev_bindings = (parse_binding ())::rev_bindings in
    match (_peek_token_exn s []).token_desc with
    | And -> _skip_next_token s; go rev_bindings
    | _ -> List.rev rev_bindings
  in
  _skip_next_token_expect s Let;
  let rec_flag = parse_rec_flag () in
  (go [], rec_flag)

and _parse_if_expr (s : _tok_stream) : Ast.expression =
  let if_tok = _get_next_token_expect s If in
  let cond_expr = _parse_expr s in
  _skip_next_token_expect s Then;
  let then_expr = _parse_expr s in
  _skip_next_token_expect s Else;
  let else_expr = _parse_expr s in
  { Ast.expr_desc = Exp_if (cond_expr, then_expr, else_expr);
    expr_span = (Span.merge if_tok.token_span else_expr.expr_span) }

and _parse_logical_or_expr (* right-associative to speed up short-circuit *)
    (s : _tok_stream) : Ast.expression =
  let lhs_expr = _parse_logical_and_expr s in
  match s.peek () with 
  | Some tok when tok.token_desc = BarBar -> 
    _skip_next_token s;
    let rhs_expr = _parse_logical_or_expr s in (* right-assoc *)
    { Ast.expr_desc = Exp_binop (Binop_or, lhs_expr, rhs_expr);
      expr_span = (Span.merge lhs_expr.expr_span rhs_expr.expr_span) }
  | _ -> lhs_expr

and _parse_logical_and_expr (* right-associative to speed up short-circuit *)
    (s : _tok_stream) : Ast.expression =
  let lhs_expr = _parse_add_sub_expr s in
  match s.peek () with 
  | Some tok when tok.token_desc = AmperAmper -> 
    _skip_next_token s;
    let rhs_expr = _parse_logical_and_expr s in (* right-assoc *)
    { Ast.expr_desc = Exp_binop (Binop_and, lhs_expr, rhs_expr);
      expr_span = (Span.merge lhs_expr.expr_span rhs_expr.expr_span) }
  | _ -> lhs_expr

and _parse_relational_expr (* left-associative *)
    (s : _tok_stream) : Ast.expression =
  let rec go lhs_expr =
    match s.peek () with 
    | Some tok when tok.token_desc = Equal || tok.token_desc = Less ->
      let binop =
        if tok.token_desc = Equal
        then Ast.Binop_eq else Ast.Binop_less in
      _skip_next_token s;
      let rhs_expr = _parse_add_sub_expr s in
      let lhs_expr = (* left-assoc *)
      { Ast.expr_desc = Exp_binop (binop, lhs_expr, rhs_expr);
        expr_span = (Span.merge lhs_expr.expr_span rhs_expr.expr_span) }
      in
      go lhs_expr
    | _ -> lhs_expr
  in
  go (_parse_add_sub_expr s)

and _parse_add_sub_expr (* left-associative *)
    (s : _tok_stream) : Ast.expression =
  let rec go lhs_expr =
    match s.peek () with 
    | Some tok when tok.token_desc = Plus || tok.token_desc = Minus ->
      let binop =
        if tok.token_desc = Plus
        then Ast.Binop_add else Ast.Binop_sub in
      _skip_next_token s;
      let rhs_expr = _parse_mul_expr s in
      let lhs_expr = (* left-assoc *)
      { Ast.expr_desc = Exp_binop (binop, lhs_expr, rhs_expr);
        expr_span = (Span.merge lhs_expr.expr_span rhs_expr.expr_span) }
      in
      go lhs_expr
    | _ -> lhs_expr
  in
  go (_parse_mul_expr s)

and _parse_mul_expr (* left-associative *)
    (s : _tok_stream) : Ast.expression =
  let rec go lhs_expr =
    match s.peek () with 
    | Some tok when tok.token_desc = Asterisk ->
      _skip_next_token s;
      let rhs_expr = _parse_apply_expr s in
      let lhs_expr = (* left-assoc *)
      { Ast.expr_desc = Exp_binop (Binop_mul, lhs_expr, rhs_expr);
        expr_span = (Span.merge lhs_expr.expr_span rhs_expr.expr_span) }
      in
      go lhs_expr
    | _ -> lhs_expr
  in
  go (_parse_apply_expr s)

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
    | Some (
        { Token.token_desc =
            (* NOTE this case must agree with _parse_primary_expr *)
            (True | False | Int _ | DecapIdent _ | Lparen); _ })
      ->
      collect_args ((_parse_primary_expr s)::rev_args)
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
    _skip_next_token s;
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
  _skip_next_token s;
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
  let bindings, rec_flag = _parse_let_bindings s in
  let expected = Token.[SemiSemiColon; In] in (* NOTE stay synched! *)
  let tok = _peek_token_exn s expected in
  _skip_next_token s;
  match tok.token_desc with
  | SemiSemiColon ->
    { Ast.struct_item_desc = Struct_bind (rec_flag, bindings);
      struct_item_span = Span.merge let_tok.token_span tok.token_span }
  | In -> (* NOTE needs to agree with _parse_let_expr *)
    let body_expr = _parse_expr s in
    let let_expr =
      { Ast.expr_desc = Exp_let (rec_flag, bindings, body_expr);
        expr_span = (Span.merge let_tok.token_span body_expr.expr_span) } in
    { Ast.struct_item_desc = Struct_eval let_expr
    ; struct_item_span = let_expr.expr_span }
  | _ -> _error_unexpected_token tok expected
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
  match s.peek () with
  | None -> []
  | Some tok ->
    let item = _parse_struct_item s in
    item::(_parse_structure s)
;;

let parse (s : token_stream) =
  try 
    Ok(_parse_structure (_create_tok_stream s))
  with Parser_error(err) -> Error err
;;