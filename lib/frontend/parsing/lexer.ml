open Pervasives

(** A [t] represents the state of lexer *)
type t = 
  { content  : string (* source to be lexed *)

  (* content[bgn_idx] is what the next token starts with *)
  ; mutable bgn_idx  : int

  (* content[cur_idx] is what we last looked at (not peeked!) *)
  ; mutable cur_idx  : int

  (* cur_loc is where we last looked at (not peeked!) *)
  ; mutable cur_loc  : Location.t
  }


exception Lexer_error of Errors.lexer_error
let _error (err : Errors.lexer_error) : 'a =
  raise (Lexer_error err)
;;

(* ASSUME [t] hasn't encountered EOF *)
let _lexer_error_expect_ch (expected : char) t : 'a =
  let cur_ch = String.get t.content t.cur_idx in
  _error (Errors.Lexer_unexpected_char (expected, cur_ch, t.cur_loc))
;;

let _lexer_error_eof (expected : char) t : 'a =
  _error (Errors.Lexer_unexpected_eof (t.cur_loc, expected))
;;

let _lexer_error_invalid_start (start : char) t : 'a =
  _error (Errors.Lexer_invalid_start (start, t.cur_loc))
;;


(* These keywords are subset of identifier with more than 1 character. *)
let _keyword_map : (string, Token.desc) Map.t =
  List.fold_right
    (fun (k, v) -> Map.add k v)
    [
      ("if",   Token.If);
      ("then", Token.Then);
      ("else", Token.Else);
      ("let",  Token.Let);
      ("rec",  Token.Rec);
      ("and",  Token.And);
      ("in",   Token.In);
      ("fun",  Token.Fun);
      ("true",  Token.True);
      ("false",  Token.False);
    ]
    (String.empty_map ())
;;

(* Used for lexing operators *)
let _symbol_chars =
  ['!'; '$'; '%'; '&'; '*'; '+'; '-'; '.'; '/'; ':'; '<'; '='; '>'; '?'; '@';
   '^'; '|'; '~';]
;;


let _get_keywd_or_iden_token_desc (str : string) : Token.desc =
  match Map.get str _keyword_map with
  | None     -> Token.DecapIdent str
  | Some tok -> tok
;;

let _get_token_str t : string =
  String.sub t.content t.bgn_idx (t.cur_idx + 1 - t.bgn_idx)
;;

let _can_be_ident_start (ch : char) : bool =
  (Char.is_alpha ch) || (ch = '_')
;;

let _can_be_ident (ch : char) : bool =
  (Char.is_alphanum ch) || (ch = '_')
;;

(* skip to a newline if [t.cur_idx] points to '\n' *)
let _get_next_cur_loc t : Location.t =
  if t.cur_idx <> -1 && (String.get t.content t.cur_idx) = '\n'
  then Location.skip_line t.cur_loc
  else Location.advance t.cur_loc;
;;

let _increment_cur_pos t : unit =
  t.cur_loc <- _get_next_cur_loc t;
  t.cur_idx <- t.cur_idx + 1;
;;

(* Return [None] on EOF, no side effect. *)
let _peek_ch t : char option =
  if (t.cur_idx + 1 == String.length t.content) then None
  else Some (String.get t.content (t.cur_idx + 1))
;;

(* Return [None] on EOF *)
let _next_ch t : char option =
  match _peek_ch t with
  | None -> None
  | Some ch -> (_increment_cur_pos t); (Some ch)
;;

(* NOTE [_consume_X] ensures that [t.cur_idx] points before the next non-X or
 * stays unchanged. *)
let rec _consume_num t : unit =
  match _peek_ch t with
  | Some ch when (Char.is_num ch) -> _increment_cur_pos t; _consume_num t
  | _ -> ()
;;

let rec _consume_ident t : unit =
  match _peek_ch t with
  | Some ch when _can_be_ident ch -> _increment_cur_pos t; _consume_ident t
  | _ -> ()
;;


(* NOTE [_cont_X t] assumes the last character(s) examined by [t] determine a
 * unique start of [X] so we "continue" to lex [X] *)
let _cont_num t : Token.desc =
  _consume_num t;
  Token.Int (_get_token_str t)
;;

let _cont_ident_or_keywd t : Token.desc =
  _consume_ident t;
  _get_keywd_or_iden_token_desc (_get_token_str t)
;;

let _cont_underscore_or_ident t : Token.desc =
  match _peek_ch t with
  | Some ch when _can_be_ident_start ch -> _cont_ident_or_keywd t
  | _ -> Token.Underscore
;;

let _cont_quote_ident t : Token.desc =
  _increment_cur_pos t; (* skip the quote *)
  _consume_ident t;
  let name = _get_token_str t in
  let name_wo_quote = String.sub name 1 ((String.length name) - 1) in
  Token.QuoteIdent name_wo_quote
;;

(* The next char in [t] should be [expect], and it'll be lexed as [tok]. *)
let _cont_expect_ch t (expect : char) (tok : Token.desc)
  : Token.desc =
  match _next_ch t with (* No need to peek if we error on mismatch *)
  | Some ch ->
    if ch = expect then tok
    else _lexer_error_expect_ch expect t
  | None -> _lexer_error_eof expect t
;;

let rec _consume_symbol_chars t : unit =
  match _peek_ch t with
  | Some ch when (List.mem ch _symbol_chars) ->
    _increment_cur_pos t;
    _consume_symbol_chars t
  | _ -> ()
;;

let _cont_infix_helper t (infix_maker : string -> Token.desc) : Token.desc =
  _consume_symbol_chars t;
  infix_maker (_get_token_str t)
;;

let _cont_infix0 t = _cont_infix_helper t (fun s -> Token.InfixOp0 s)
;;
let _cont_infix1 t = _cont_infix_helper t (fun s -> Token.InfixOp1 s)
;;
let _cont_infix2 t = _cont_infix_helper t (fun s -> Token.InfixOp2 s)
;;
let _cont_infix3 t = _cont_infix_helper t (fun s -> Token.InfixOp3 s)
;;
  
let _cont_equal_or_infix0 t : Token.desc =
  match _peek_ch t with
  | Some ch when List.mem ch _symbol_chars -> _cont_infix0 t
  | _ -> Token.Equal
;;

let _cont_amperamper_or_infix0 t : Token.desc =
  match _peek_ch t with
  | Some '&' -> _increment_cur_pos t; Token.AmperAmper
  | _ -> _cont_infix0 t
;;

let _cont_barbar_or_infix0 t : Token.desc =
  match _peek_ch t with
  | Some '|' -> _increment_cur_pos t; Token.BarBar
  | _ -> _cont_infix0 t
;;

let _cont_or_arrow_or_infix2 t : Token.desc =
  match _peek_ch t with
  | Some '>' -> _increment_cur_pos t; Token.Rarrow
  | _ -> _cont_infix2 t
;;

(* ASSUME 
 * 1. space has been skipped.
 * 2. [cur_ch] is the [t.cur_idx]th char in [t.contents] *)
let _lex_with_cur_ch t (cur_ch : char) : Token.desc =
  match cur_ch with
  | '_' -> _cont_underscore_or_ident t
  | '\'' -> _cont_quote_ident t
  | ';' -> _cont_expect_ch t cur_ch Token.SemiSemiColon
  | ':' -> Token.Colon
  | '(' -> Token.Lparen
  | ')' -> Token.Rparen
  | '='             -> _cont_equal_or_infix0 t
  | '|'             -> _cont_amperamper_or_infix0 t
  | '&'             -> _cont_barbar_or_infix0 t
  | '<' | '>' | '$' -> _cont_infix0 t
  | '@' | '^'       -> _cont_infix1 t
  | '-'             -> _cont_or_arrow_or_infix2 t
  | '+'             -> _cont_infix2 t
  | '*' | '/' | '%' -> _cont_infix3 t
  | _ when (Char.is_num cur_ch) -> _cont_num t
  | _ when _can_be_ident_start cur_ch -> _cont_ident_or_keywd t
  | _ -> _lexer_error_invalid_start cur_ch t
;;


(* ENSURE:
 * [t.bgn_idx] points to the 1st non-space;
 * [t.cur_idx] points right before [t.bgn_idx]. *)
let rec _lex_skip_space t : unit =
  match _peek_ch t with
  | Some (' ' | '\t' | '\n') ->
    _increment_cur_pos t;
    _lex_skip_space t
  | _ -> t.bgn_idx <- t.cur_idx + 1 (* not a space, start of new token *)
;;

let _next t =
  _lex_skip_space t;
  match _next_ch t with
  | None -> None
  | Some (ch) ->
    let start_loc = t.cur_loc in
    let token_desc = _lex_with_cur_ch t ch in
    let token_span = Span.create start_loc t.cur_loc in
    t.bgn_idx <- t.cur_idx + 1;
    Some { Token.token_desc; token_span }
;;

let _lex_all_tokens t : Token.t list =
  let rec go toks =
    match _next t with
    | None -> List.rev toks
    | Some (tok) -> go (tok::toks)
  in
  go []
;;

let lex str =
  if str = "" then Ok []
  else
    let t = { content = str; bgn_idx = 0; cur_idx = -1
            ; cur_loc = Location.create 1 0 }
    in
    try Ok (_lex_all_tokens t)
    with Lexer_error err -> Error err
;;
