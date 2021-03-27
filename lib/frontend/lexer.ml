open Pervasives

type t = 
  { filename : string
  ; content  : string (* content of file with [filename] *)

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

let _lexer_error_eof (what : Errors.lexer_action) t : 'a =
  _error (Errors.Lexer_unexpected_eof (t.cur_loc, what))
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
    (Map.empty String.compare)
;;


let _get_keywd_or_iden_token_desc (str : string) : Token.desc =
  match Map.get str _keyword_map with
  | None     -> Token.DecapIdent str
  | Some tok -> tok
;;

let _get_token_str t : string =
  String.sub t.content t.bgn_idx (t.cur_idx + 1 - t.bgn_idx)
;;

let _can_be_ident (ch : char) : bool =
  (Char.is_alpha ch) || (ch = '_')
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


(* NOTE [_cont_X t] assumes the last character(s) examined by [t] determine a
 * unique start of [X] so we "continue" to lex [X] *)

let rec _cont_num t : Token.desc =
  match _peek_ch t with
  | Some ch when (Char.is_num ch) -> _increment_cur_pos t; _cont_num t
  | Some _ -> Token.Int (_get_token_str t)
  | None -> _lexer_error_eof Errors.Lexing_number t
;;

let rec _cont_ident_or_keywd t : Token.desc =
  match _peek_ch t with
  | Some ch when _can_be_ident ch -> _increment_cur_pos t; _cont_ident_or_keywd t
  | Some _ -> _get_keywd_or_iden_token_desc (_get_token_str t)
  | None -> _lexer_error_eof Errors.Lexing_identifier_or_keyword t
;;

let _cont_minus_or_arrow t : Token.desc =
  match _peek_ch t with
  | Some '>' -> _increment_cur_pos t; Token.Rarrow
  | _ -> Token.Minus
;;

(* The next char in [t] should be [expect], and it'll be lexed as [tok]. *)
let _cont_expect_ch t (expect : char) (tok : Token.desc)
  : Token.desc =
  match _next_ch t with (* No need to peek if we error on mismatch *)
  | Some ch ->
    if ch = expect then tok
    else _lexer_error_expect_ch expect t
  | None -> _lexer_error_eof (Errors.Lexing_expecting expect) t
;;

(* ASSUME 
 * 1. space has been skipped.
 * 2. [cur_ch] is the [t.cur_idx]th char in [t.contents] *)
let _lex_with_cur_ch t (cur_ch : char) : Token.desc =
  match cur_ch with
  | _ when (Char.is_num cur_ch) -> _cont_num t
  | _ when _can_be_ident cur_ch -> _cont_ident_or_keywd t
  | '-' -> _cont_minus_or_arrow t
           (* XXX Might have to change if say, ";" becomes a valid token *)
  | ';' -> _cont_expect_ch t cur_ch Token.SemiSemiColon
  | '&' -> _cont_expect_ch t cur_ch Token.AmperAmper
  | '|' -> _cont_expect_ch t cur_ch Token.BarBar
  | '+' -> Token.Plus
  | '*' -> Token.Asterisk
  | ':' -> Token.Colon
  | '=' -> Token.Equal
  | '(' -> Token.Lparen
  | ')' -> Token.Rparen
  | '<' -> Token.Less
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


let create filename =
  let content = Io.read_file filename in
  { filename; content;
    bgn_idx = 0; cur_idx = -1; cur_loc = Location.create 0 ~-1 }
;;

let next t =
  _lex_skip_space t;
  match _next_ch t with
  | None -> None
  | Some (ch) ->
    let start_loc = t.cur_loc in
    let token_desc = _lex_with_cur_ch t ch in
    let token_span = Span.create t.filename start_loc t.cur_loc in
    t.bgn_idx <- t.cur_idx + 1;
    Some { Token.token_desc; token_span }
;;

let next_loc t = _get_next_cur_loc t
;;
