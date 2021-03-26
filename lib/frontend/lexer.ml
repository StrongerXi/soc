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


(* These keywords are subset of identifier with more than 1 character. *)
let _keyword_map : (string, Parser.token_desc) Map.t =
  List.fold_right
    (fun (k, v) -> Map.add k v)
    [
      ("if",   Parser.If);
      ("then", Parser.Then);
      ("else", Parser.Else);
      ("let",  Parser.Let);
      ("rec",  Parser.Rec);
      ("and",  Parser.And);
      ("in",   Parser.In);
      ("fun",  Parser.Fun);
      ("true",  Parser.True);
      ("false",  Parser.False);
    ]
    (Map.empty String.compare)
;;


let _get_keywd_or_iden_token_desc (str : string) : Parser.token_desc =
  match Map.get str _keyword_map with
  | None     -> Parser.DecapIdent str
  | Some tok -> tok
;;

let _get_token_str t : string =
  String.sub t.content t.bgn_idx (t.cur_idx + 1 - t.bgn_idx)
;;

let _can_be_ident (ch : char) : bool =
  (Char.is_alpha ch) || (ch = '_')
;;

(* [printf "%s at (%d, %d)" reason t.cur_row t.cur_col] *)
let _lexer_error_general reason t : 'a =
  let msg = String.append reason " at " in
  let msg = String.append msg (Location.to_string t.cur_loc) in
  failwith msg
;;

(* [printf "Expected '%c', but got '%c' at (%d, %d)" ...]
 * ASSUME [t] hasn't encountered EOF *)
let _lexer_error_expect_ch (expected : char) t : 'a =
  let reason = String.append "Expected '" (Char.to_string expected) in
  let reason = String.append reason "', but got '" in
  let cur_ch = String.get t.content t.cur_idx in
  let reason = String.append reason (Char.to_string cur_ch) in
  let reason = String.append reason "'" in
  _lexer_error_general reason t
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

let rec _cont_num t : Parser.token_desc =
  match _peek_ch t with
  | Some ch when (Char.is_num ch) -> _increment_cur_pos t; _cont_num t
  | Some ch -> Parser.Int (_get_token_str t)
  | None -> _lexer_error_general "Unexpected EOF while lexing a number" t
;;

let rec _cont_ident_or_keywd t : Parser.token_desc =
  match _peek_ch t with
  | Some ch when _can_be_ident ch -> _increment_cur_pos t; _cont_ident_or_keywd t
  | Some ch -> _get_keywd_or_iden_token_desc (_get_token_str t)
  | None -> _lexer_error_general "Unexpected EOF while lexing an identifier" t
;;

let _cont_minus_or_arrow t : Parser.token_desc =
  match _peek_ch t with
  | Some '>' -> _increment_cur_pos t; Parser.Rarrow
  | _ -> Parser.Minus
;;

(* The next char in [t] should be [expect], and it'll be lexed as [tok]. *)
let _cont_expect_ch t (expect : char) (tok : Parser.token_desc)
  : Parser.token_desc =
  match _next_ch t with (* No need to peek if we error on mismatch *)
  | Some ch ->
    if ch = expect then tok
    else _lexer_error_expect_ch expect t
  | None ->
    let reason = "Unexpected EOF while expecting '" in
    let reason = String.append reason (Char.to_string expect) in
    let reason = String.append reason "'" in
    _lexer_error_general reason t
;;

(* ASSUME 
 * 1. space has been skipped.
 * 2. [cur_ch] is the [t.cur_idx]th char in [t.contents] *)
let _lex_with_cur_ch t (cur_ch : char) : Parser.token_desc =
  match cur_ch with
  | _ when (Char.is_num cur_ch) -> _cont_num t
  | _ when _can_be_ident cur_ch -> _cont_ident_or_keywd t
  | '-' -> _cont_minus_or_arrow t
  | ';' -> _cont_expect_ch t cur_ch Parser.SemiSemiColon
  | '&' -> _cont_expect_ch t cur_ch Parser.AmperAmper
  | '|' -> _cont_expect_ch t cur_ch Parser.BarBar
  | '+' -> Parser.Plus
  | '*' -> Parser.Asterisk
  | ':' -> Parser.Colon
  | '=' -> Parser.Equal
  | '(' -> Parser.Lparen
  | ')' -> Parser.Rparen
  | '<' -> Parser.Less
  | _ -> 
    let reason =
      String.append "Unknown start of token" (Char.to_string cur_ch) in
    _lexer_error_general reason t
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
  let content = Externals.read_entire_file filename in
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
    Some { Parser.token_desc; token_span }
;;

let next_loc t = _get_next_cur_loc t
;;
